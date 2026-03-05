#!/bin/bash
# SSH-based deployment script for FuelsCraft-web
# Usage: ./deploy_ssh.sh [user@host] [action]
# Example: ./deploy_ssh.sh ubuntu@server.washington.edu deploy

set -e

if [ $# -lt 1 ]; then
    echo "Usage: ./deploy_ssh.sh <user@host> [action]"
    echo ""
    echo "Actions:"
    echo "  deploy      - Full deployment (default)"
    echo "  build       - Build Docker image only"
    echo "  stop        - Stop running container"
    echo "  start       - Start container"
    echo "  logs        - View container logs"
    echo "  status      - Show container status"
    echo "  clean       - Clean up Docker resources"
    echo "  health      - Run health check"
    echo ""
    exit 1
fi

HOST=$1
ACTION=${2:-deploy}

echo "========================================"
echo "FuelsCraft-web Deployment (SSH)"
echo "========================================"
echo "Host: $HOST"
echo "Action: $ACTION"
echo ""

# Check if SSH key is available
if [ ! -f ~/.ssh/id_rsa ]; then
    echo "✗ SSH key not found at ~/.ssh/id_rsa"
    exit 1
fi

# Ensure SSH key is added to agent (will prompt for passphrase if needed)
# Use -K flag on macOS to store passphrase in keychain
if [[ "$OSTYPE" == "darwin"* ]]; then
    ssh-add -K ~/.ssh/id_rsa 2>/dev/null || ssh-add ~/.ssh/id_rsa 2>/dev/null || true
else
    ssh-add ~/.ssh/id_rsa 2>/dev/null || true
fi

case $ACTION in
    deploy)
        echo "Running full deployment..."
        ssh -o ConnectTimeout=10 "$HOST" 'bash -s' << 'EOF'
            set -e
            echo "Step 1: Pulling latest code..."
            cd /home/ubuntu/fuels-craft-web || (mkdir -p /home/ubuntu && cd /home/ubuntu && git clone https://github.com/uw-flame-lab/fuels-craft.git fuels-craft-web)
            cd /home/ubuntu/fuels-craft-web/shinyApps/phase1/FuelsCraft-web
            git pull origin main

            echo "Step 2: Building Docker image..."
            docker build -t fuels-craft-web:latest .

            echo "Step 3: Stopping old container..."
            docker stop fuels-craft-web 2>/dev/null || true
            docker rm fuels-craft-web 2>/dev/null || true

            echo "Step 4: Starting new container..."
            docker run -d --restart always -p 3001:3001 --name fuels-craft-web fuels-craft-web:latest

            echo "Step 5: Checking health..."
            sleep 2
            curl -s http://localhost:3001 | head -c 100
            echo ""
            echo "✓ Deployment complete!"
EOF
        ;;
    build)
        echo "Building Docker image..."
        ssh -o ConnectTimeout=10 "$HOST" 'cd /home/ubuntu/fuels-craft-web/shinyApps/phase1/FuelsCraft-web && docker build -t fuels-craft-web:latest .'
        ;;
    stop)
        echo "Stopping container..."
        ssh -o ConnectTimeout=10 "$HOST" 'docker stop fuels-craft-web 2>/dev/null || true && docker rm fuels-craft-web 2>/dev/null || true'
        echo "✓ Container stopped"
        ;;
    start)
        echo "Starting container..."
        ssh -o ConnectTimeout=10 "$HOST" 'docker run -d --restart always -p 3001:3001 --name fuels-craft-web fuels-craft-web:latest'
        echo "✓ Container started"
        ;;
    logs)
        echo "Showing logs (press Ctrl+C to exit)..."
        ssh -o ConnectTimeout=10 "$HOST" 'docker logs -f fuels-craft-web'
        ;;
    status)
        echo "Docker containers:"
        ssh -o ConnectTimeout=10 "$HOST" 'docker ps'
        ;;
    clean)
        echo "Cleaning up Docker resources..."
        ssh -o ConnectTimeout=10 "$HOST" 'docker system prune -f'
        echo "✓ Cleanup complete"
        ;;
    health)
        echo "Checking health..."
        ssh -o ConnectTimeout=10 "$HOST" 'curl -s http://localhost:3001 | head -c 100'
        echo ""
        ;;
    *)
        echo "✗ Unknown action: $ACTION"
        exit 1
        ;;
esac
