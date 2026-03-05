"""
Fabric deployment script for FuelsCraft-web
Usage: fab -H user@host deploy
"""

from fabric import task, Connection
from pathlib import Path
import os
from datetime import datetime

# Configuration
REPO_URL = "https://github.com/uw-flame-lab/fuels-craft.git"
PROJECT_PATH = "/opt/fuels-craft-web"
PROJECT_NAME = "fuels-craft-web"
IMAGE_NAME = "fuels-craft-web"
CONTAINER_NAME = "fuels-craft-web"
PORT = 3001

# Environment variables (customize as needed)
ENV_VARS = {
    "NODE_ENV": "production",
    "PORT": "3001",
    "LOG_LEVEL": "info",
}


@task
def health_check(c):
    """Check the health of the deployment"""
    try:
        result = c.run(f"curl -s http://localhost:{PORT} | head -c 100", hide=True)
        print(f"✓ Application is running on port {PORT}")
        return True
    except Exception as e:
        print(f"✗ Application health check failed: {e}")
        return False


@task
def stop(c):
    """Stop the running container"""
    print("Stopping container...")
    c.run(f"docker stop {CONTAINER_NAME} 2>/dev/null || true", warn=True)
    c.run(f"docker rm {CONTAINER_NAME} 2>/dev/null || true", warn=True)
    print("✓ Container stopped")


@task
def remove_image(c):
    """Remove the Docker image"""
    print("Removing old image...")
    c.run(f"docker rmi {IMAGE_NAME}:latest 2>/dev/null || true", warn=True)
    print("✓ Image removed")


@task
def build(c):
    """Build Docker image"""
    print(f"Building Docker image: {IMAGE_NAME}...")
    c.run(
        f"cd {PROJECT_PATH}/shinyApps/phase1/FuelsCraft-web && "
        f"docker build -t {IMAGE_NAME}:latest .",
        pty=True
    )
    print("✓ Image built successfully")


@task
def pull_code(c):
    """Pull latest code from repository"""
    print("Pulling latest code...")

    # Check if repo exists, otherwise clone
    if c.run(f"test -d {PROJECT_PATH}/.git", warn=True).ok:
        c.run(f"cd {PROJECT_PATH} && git pull origin main", pty=True)
        print("✓ Code updated")
    else:
        c.run(f"git clone {REPO_URL} {PROJECT_PATH}", pty=True)
        print("✓ Repository cloned")


@task
def run(c):
    """Run Docker container"""
    print(f"Starting container: {CONTAINER_NAME}...")

    # Build environment variables string
    env_string = " ".join([f"-e {k}={v}" for k, v in ENV_VARS.items()])

    c.run(
        f"docker run -d "
        f"--name {CONTAINER_NAME} "
        f"--restart unless-stopped "
        f"-p {PORT}:{PORT} "
        f"{env_string} "
        f"{IMAGE_NAME}:latest",
        pty=True
    )
    print(f"✓ Container started on port {PORT}")


@task
def logs(c):
    """View container logs"""
    print(f"Showing logs for {CONTAINER_NAME}...")
    c.run(f"docker logs -f {CONTAINER_NAME}", pty=True)


@task
def ps(c):
    """Show running containers"""
    print("Running containers:")
    c.run("docker ps", pty=True)


@task
def deploy(c):
    """
    Full deployment workflow
    Usage: fab -H user@host deploy
    """
    print(f"\n{'='*60}")
    print(f"Deploying {PROJECT_NAME}")
    print(f"{'='*60}\n")

    # 1. Pull latest code
    print("Step 1: Pulling latest code")
    pull_code(c)
    print()

    # 2. Build Docker image
    print("Step 2: Building Docker image")
    build(c)
    print()

    # 3. Stop old container
    print("Step 3: Stopping old container")
    stop(c)
    print()

    # 4. Start new container
    print("Step 4: Starting new container")
    run(c)
    print()

    # 5. Health check
    print("Step 5: Running health check")
    import time
    time.sleep(3)  # Wait for container to start

    if health_check(c):
        print(f"\n{'='*60}")
        print("✓ DEPLOYMENT SUCCESSFUL!")
        print(f"Application available at: http://{c.host}:{PORT}")
        print(f"{'='*60}\n")
    else:
        print("\n⚠ Deployment completed but health check failed")
        print("Check logs with: fab -H user@host logs")


@task
def rollback(c):
    """
    Rollback to previous image version
    Note: Requires docker image retention policy
    """
    print("Rolling back to previous version...")
    stop(c)

    # Try to find previous image
    result = c.run("docker images | grep fuels-craft-web | head -2", hide=True, warn=True)
    if len(result.stdout.strip().split('\n')) > 1:
        lines = result.stdout.strip().split('\n')
        prev_image = lines[1].split()[2]  # Get image ID
        c.run(f"docker tag {prev_image} {IMAGE_NAME}:latest", warn=True)
        run(c)
        print("✓ Rolled back to previous version")
    else:
        print("✗ No previous image found for rollback")


@task
def clean(c):
    """Remove unused Docker images and containers"""
    print("Cleaning up Docker resources...")
    c.run("docker container prune -f", warn=True)
    c.run("docker image prune -f", warn=True)
    print("✓ Cleanup complete")


@task
def shell(c):
    """Open shell in running container"""
    print(f"Opening shell in {CONTAINER_NAME}...")
    c.run(f"docker exec -it {CONTAINER_NAME} /bin/sh", pty=True)


@task
def version(c):
    """Show version info"""
    print(f"Project: {PROJECT_NAME}")
    print(f"Container: {CONTAINER_NAME}")
    print(f"Image: {IMAGE_NAME}")
    print(f"Port: {PORT}")
    print(f"Repo: {REPO_URL}")

    result = c.run(f"cd {PROJECT_PATH} && git rev-parse --short HEAD 2>/dev/null || echo 'N/A'",
                   hide=True, warn=True)
    print(f"Latest commit: {result.stdout.strip()}")
