# FuelsCraft-web Deployment Guide

## Quick Start

### Prerequisites
- Docker and Docker Compose installed
- Fabric3 installed: `pip install fabric`
- Git access to repository

### Local Testing with Docker Compose

```bash
# Build and start locally
docker-compose up --build

# Access at http://localhost:3001
```

---

## Remote Deployment with Fabric

### 1. Install Fabric on your local machine
```bash
pip install fabric
```

### 2. Basic Commands

**Full deployment:**
```bash
fab -H user@your-uw-server.washington.edu deploy
```

**Individual tasks:**
```bash
# Pull latest code
fab -H user@host pull_code

# Build Docker image
fab -H user@host build

# Stop running container
fab -H user@host stop

# Start new container
fab -H user@host run

# View logs
fab -H user@host logs

# Check system status
fab -H user@host ps

# Health check
fab -H user@host health_check
```

---

## Manual Docker Deployment

### Build
```bash
cd /path/to/FuelsCraft-web
docker build -t fuels-craft-web:latest .
```

### Run
```bash
docker run -d \
  --name fuels-craft-web \
  --restart unless-stopped \
  -p 3001:3001 \
  -e NODE_ENV=production \
  fuels-craft-web:latest
```

### Monitor
```bash
# View logs
docker logs -f fuels-craft-web

# Check status
docker ps

# Health check
curl http://localhost:3001
```

### Stop and Remove
```bash
docker stop fuels-craft-web
docker rm fuels-craft-web
docker rmi fuels-craft-web:latest
```

---

## Environment Configuration

Set environment variables in fabfile.py or pass them to docker:

```python
ENV_VARS = {
    "NODE_ENV": "production",
    "PORT": "3001",
    "LOG_LEVEL": "info",
    "FASTFUELS_API_URL": "https://api.fastfuels.silvxlabs.com",
    # Add more as needed
}
```

---

## Troubleshooting

### Container won't start
```bash
docker logs fuels-craft-web
```

### Port already in use
```bash
# Find process on port 3001
lsof -i :3001

# Kill process
kill -9 <PID>

# Or change port in docker run command
docker run -p 3002:3001 ...
```

### Rebuild without cache
```bash
docker build --no-cache -t fuels-craft-web:latest .
```

### View running processes
```bash
fab -H user@host ps
```

---

## Production Checklist

- [ ] Update NODE_ENV to "production"
- [ ] Set LOG_LEVEL appropriately
- [ ] Configure HTTPS/SSL (via reverse proxy)
- [ ] Set up monitoring and logging
- [ ] Configure backups for data files
- [ ] Test health checks
- [ ] Set up automatic restart policy
- [ ] Document any custom environment variables
- [ ] Test rollback procedure

---

## Nginx Reverse Proxy (Optional)

If deploying behind Nginx on the same server:

```nginx
server {
    listen 80;
    server_name fuels-craft.example.com;

    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name fuels-craft.example.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location / {
        proxy_pass http://localhost:3001;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

---

## Monitoring & Maintenance

### Log Rotation
Docker automatically handles log rotation. Configure in daemon.json:

```json
{
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "10m",
    "max-file": "3"
  }
}
```

### Automated Backups
Add to crontab for scheduled backups:

```cron
# Backup uploaded tree inventory files daily at 2 AM
0 2 * * * tar -czf /backups/fuels-craft-$(date +\%Y\%m\%d).tar.gz /opt/fuels-craft-web/
```

---

## Rollback Procedure

```bash
# Stop current version
fab -H user@host stop

# Rollback to previous image
fab -H user@host rollback

# Verify
fab -H user@host health_check
```

---

## Performance Optimization

### Build optimization
- Using multi-stage build to reduce image size
- Alpine Linux base image (lightweight)
- Production dependencies only

### Runtime optimization
- Non-root user for security
- Signal handling with dumb-init
- Automatic restart policy

---

## Support

For issues:
1. Check logs: `docker logs fuels-craft-web`
2. Verify network: `docker exec fuels-craft-web curl http://localhost:3001`
3. Check disk space: `docker system df`
4. Review firewall rules

For questions, contact: bdrye@uw.edu
