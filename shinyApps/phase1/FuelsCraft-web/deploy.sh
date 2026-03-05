#!/bin/bash
# Quick deployment script for FuelsCraft-web
# Usage: ./deploy.sh [user@host] [action]
# Example: ./deploy.sh user@server.washington.edu deploy

set -e

if [ $# -lt 1 ]; then
    echo "Usage: ./deploy.sh <user@host> [action]"
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
    echo "  rollback    - Rollback to previous version"
    echo ""
    exit 1
fi

HOST=$1
ACTION=${2:-deploy}

echo "========================================"
echo "FuelsCraft-web Deployment"
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

# Use SSH_AUTH_SOCK for all invocations
export SSH_AUTH_SOCK

case $ACTION in
    deploy)
        echo "Running full deployment..."
        fab -H "$HOST" deploy
        ;;
    build)
        echo "Building Docker image..."
        fab -H "$HOST" build
        ;;
    stop)
        echo "Stopping container..."
        fab -H "$HOST" stop
        ;;
    start)
        echo "Starting container..."
        fab -H "$HOST" run
        ;;
    logs)
        echo "Showing logs..."
        fab -H "$HOST" logs
        ;;
    status|ps)
        echo "Checking status..."
        fab -H "$HOST" ps
        ;;
    clean)
        echo "Cleaning up..."
        fab -H "$HOST" clean
        ;;
    health)
        echo "Running health check..."
        fab -H "$HOST" health_check
        ;;
    rollback)
        echo "Rolling back..."
        fab -H "$HOST" rollback
        ;;
    *)
        echo "Unknown action: $ACTION"
        exit 1
        ;;
esac

echo ""
echo "Done!"
