# FuelsCraft-web Deployment Files

This directory contains all files needed for easy deployment of FuelsCraft-web.

## Files Included

### Core Deployment Files
- **Dockerfile** - Multi-stage build for optimized production image
- **fabfile.py** - Fabric automation for remote deployment
- **docker-compose.yml** - Local development environment
- **.dockerignore** - Optimizes Docker build

### Configuration & Documentation
- **DEPLOYMENT.md** - Comprehensive deployment guide
- **.env.example** - Environment configuration template
- **nginx.conf.example** - Nginx reverse proxy template
- **deploy.sh** - Quick deployment script wrapper

---

## Quick Start

### 1. Local Development
```bash
docker-compose up --build
```
Access at http://localhost:3001

### 2. Remote Deployment to UW Server

**First time setup:**
```bash
# Install Fabric locally
pip install fabric

# Copy .env.example to .env and customize
cp .env.example .env

# Make deploy script executable
chmod +x deploy.sh
```

**Deploy:**
```bash
# Full deployment
./deploy.sh user@your-uw-server.washington.edu deploy

# Or use Fabric directly
fab -H user@your-uw-server.washington.edu deploy
```

---

## What Each File Does

### Dockerfile
- Uses Node 18 Alpine (lightweight)
- Multi-stage build: React in first stage, server in second
- Builds frontend and includes in server's static files
- Runs as non-root user for security
- ~300MB total image size

### fabfile.py
Automated deployment tasks:
- `pull_code` - Clone or update Git repository
- `build` - Build Docker image
- `run` - Start Docker container
- `stop` - Stop and remove container
- `deploy` - Full deployment workflow
- `health_check` - Verify application is running
- `logs` - View container logs
- `rollback` - Revert to previous image
- `clean` - Remove unused Docker resources

### deploy.sh
Simple wrapper around Fabric for quick commands:
```bash
./deploy.sh user@host deploy      # Full deployment
./deploy.sh user@host logs        # View logs
./deploy.sh user@host health      # Health check
./deploy.sh user@host rollback    # Rollback
```

### docker-compose.yml
Local testing with:
- Single service definition
- Port 3001 exposed
- Health checks enabled
- Automatic restart policy
- Volume mounting for logs

### nginx.conf.example
Production reverse proxy setup:
- HTTPS/SSL configuration
- Security headers
- Gzip compression
- Rate limiting friendly
- WebSocket support

---

## Deployment Workflow

### First-time deployment to UW server:

1. **SSH to server:**
   ```bash
   ssh user@your-uw-server.washington.edu
   ```

2. **Install Docker (if not already):**
   ```bash
   # On UW-managed system, request via IT
   # Or: sudo apt-get install docker.io docker-compose
   ```

3. **Prepare deployment:**
   ```bash
   # From local machine
   ./deploy.sh user@host deploy
   ```

4. **Configure reverse proxy (optional):**
   ```bash
   sudo cp nginx.conf.example /etc/nginx/sites-available/fuels-craft
   sudo ln -s /etc/nginx/sites-available/fuels-craft /etc/nginx/sites-enabled/
   sudo nginx -t
   sudo systemctl reload nginx
   ```

### Subsequent deployments (pull, rebuild, restart):
```bash
./deploy.sh user@host deploy
```

### Just check status:
```bash
./deploy.sh user@host status
```

### View logs in real-time:
```bash
./deploy.sh user@host logs
```

---

## Environment Configuration

Copy `.env.example` to `.env` and customize:

```bash
cp .env.example .env
nano .env
```

Key variables:
- `NODE_ENV=production` - Production mode
- `PORT=3001` - Server port
- `FASTFUELS_API_KEY` - Your FastFuels API key
- `REACT_APP_API_BASE_URL` - API endpoint for frontend

---

## Monitoring & Maintenance

### View logs:
```bash
fab -H user@host logs
```

### Check container status:
```bash
fab -H user@host ps
```

### Health check:
```bash
fab -H user@host health_check
```

### Rollback on failure:
```bash
fab -H user@host rollback
```

### Clean up old images:
```bash
fab -H user@host clean
```

---

## Troubleshooting

### Connection refused
```bash
# Check if container is running
fab -H user@host ps

# View logs for errors
fab -H user@host logs
```

### Port in use
The deploy script will stop the old container first.
If still having issues:
```bash
# Find process on port 3001
sudo lsof -i :3001

# Kill it
sudo kill -9 <PID>
```

### Docker not available
Request Docker from UW-IT or eScience Institute

---

## Production Checklist

- [ ] Environment variables configured in `.env`
- [ ] FASTFUELS_API_KEY set correctly
- [ ] NODE_ENV set to "production"
- [ ] Nginx reverse proxy configured
- [ ] SSL certificates installed
- [ ] Firewall allows port 443 (HTTPS)
- [ ] Backups scheduled for data files
- [ ] Monitoring/alerting configured
- [ ] Health checks working
- [ ] Logs being collected
- [ ] Tested rollback procedure

---

## Support

For questions or issues:
1. Check DEPLOYMENT.md for detailed guide
2. Review container logs: `fab -H host logs`
3. Contact bdrye@uw.edu

## Key Contacts for UW Deployment

- **eScience Institute:** escience@uw.edu (for compute resources)
- **UW-IT Help:** 4-help@uw.edu (for infrastructure)
- **UW IT Support:** itconnect.uw.edu (for cloud services)
