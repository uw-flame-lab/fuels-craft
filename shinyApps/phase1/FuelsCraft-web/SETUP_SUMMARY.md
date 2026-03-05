# FuelsCraft-web Easy Deployment Setup

## Summary

I've created a complete Docker and Fabric deployment system for FuelsCraft-web. This makes deploying to UW servers simple and automated.

---

## What's Been Created

### 🐳 Docker Files
1. **Dockerfile** - Optimized multi-stage build
   - Builds React frontend
   - Includes in Node.js server
   - ~300MB total image size
   - Non-root security user

2. **.dockerignore** - Build optimization

3. **docker-compose.yml** - Local development
   - Quick `docker-compose up --build`
   - Pre-configured port and settings

### 🚀 Deployment Files
4. **fabfile.py** - Fabric automation (Python)
   - `deploy` - Full deployment workflow
   - `build` - Build Docker image
   - `stop/run` - Container management
   - `logs` - View real-time logs
   - `health_check` - Verify application
   - `rollback` - Revert to previous version
   - Many more utility commands

5. **deploy.sh** - Quick wrapper script
   - Simple bash interface to Fabric
   - Easy-to-remember commands

### 📋 Configuration Files
6. **.env.example** - Environment template
   - Copy to `.env` and customize
   - FastFuels API key, port, logging, etc.

7. **nginx.conf.example** - Nginx reverse proxy
   - HTTPS/SSL setup
   - Security headers
   - WebSocket support
   - Gzip compression

### 📖 Documentation
8. **DEPLOYMENT.md** - Comprehensive guide
   - Step-by-step instructions
   - Troubleshooting
   - Monitoring & maintenance
   - Production checklist

9. **DEPLOYMENT_FILES_README.md** - This system's overview
   - What each file does
   - Quick start guide
   - Deployment workflow

### 🔧 Utilities
10. **requirements-deploy.txt** - Python dependencies
    - `pip install -r requirements-deploy.txt`
    - Installs Fabric for automation

11. **.github/workflows/docker.yml** - CI/CD automation
    - Auto-builds Docker image on push
    - Pushes to GitHub Container Registry

---

## Quick Start (3 Steps)

### Step 1: Local Testing (Optional)
```bash
cd /path/to/FuelsCraft-web
docker-compose up --build
# Access at http://localhost:3001
```

### Step 2: Setup Deployment Tools (Local Machine)
```bash
pip install -r requirements-deploy.txt
chmod +x deploy.sh
cp .env.example .env
# Edit .env with your FastFuels API key
```

### Step 3: Deploy to UW Server
```bash
# Replace with your actual UW server
./deploy.sh user@your-uw-server.washington.edu deploy
```

That's it! The application will be:
- ✓ Built as Docker image
- ✓ Started in container
- ✓ Running on port 3001
- ✓ Auto-restarting on failure

---

## Common Deployment Commands

```bash
# Full deployment (build + start)
./deploy.sh user@host deploy

# View logs
./deploy.sh user@host logs

# Check status
./deploy.sh user@host status

# Rollback to previous version
./deploy.sh user@host rollback

# Health check
./deploy.sh user@host health

# Restart
./deploy.sh user@host stop && ./deploy.sh user@host start
```

Or use Fabric directly:
```bash
fab -H user@host deploy
fab -H user@host logs
fab -H user@host ps
```

---

## File Structure

```
FuelsCraft-web/
├── Dockerfile                      # Multi-stage build
├── .dockerignore                   # Build optimization
├── docker-compose.yml              # Local dev environment
├── fabfile.py                      # Deployment automation
├── deploy.sh                       # Quick deployment script
├── .env.example                    # Environment template
├── nginx.conf.example              # Reverse proxy config
├── requirements-deploy.txt         # Fabric dependencies
├── DEPLOYMENT.md                   # Comprehensive guide
├── DEPLOYMENT_FILES_README.md      # This system's docs
├── .github/workflows/
│   └── docker.yml                  # CI/CD pipeline
├── client/                         # React app
├── server/                         # Node.js backend
└── ...other files
```

---

## Key Features

### 🔒 Security
- Non-root container user
- Alpine Linux (minimal attack surface)
- Nginx security headers included
- SSL/TLS support

### 📦 Optimization
- Multi-stage Docker build
- ~300MB image size
- Production dependencies only
- Gzip compression

### 🔄 Automation
- One-command deployment
- Automatic container restart
- Health checks included
- Easy rollback

### 📊 Monitoring
- Real-time logs
- Container status checks
- Health verification
- Simple health checks

---

## Example: Deploying to UW eScience Server

```bash
# 1. Get SSH access to server from eScience
# 2. Clone repository on your local machine
git clone https://github.com/uw-flame-lab/fuels-craft.git
cd fuels-craft/shinyApps/phase1/FuelsCraft-web

# 3. Setup deployment
pip install -r requirements-deploy.txt
cp .env.example .env
nano .env  # Add your FastFuels API key

# 4. Deploy!
./deploy.sh user@compute.escience.washington.edu deploy

# 5. Monitor
./deploy.sh user@compute.escience.washington.edu logs

# 6. Access
# http://compute.escience.washington.edu:3001
```

---

## Troubleshooting Quick Tips

| Issue | Solution |
|-------|----------|
| Connection refused | Check logs: `fab -H host logs` |
| Port already in use | Old container still running: `fab -H host stop` |
| Permission denied | Add user to docker group: `sudo usermod -aG docker $USER` |
| Build fails | Check Docker space: `docker system df` |
| App not responding | Health check: `fab -H host health_check` |

---

## Next Steps

1. **Update FastFuels API Key**
   - Get from: https://app.fastfuels.silvxlabs.com/
   - Add to `.env`

2. **Choose UW Hosting**
   - eScience Institute (free)
   - Your department
   - UW-IT cloud services

3. **Setup Nginx** (optional, for HTTPS)
   - Copy `nginx.conf.example` to `/etc/nginx/sites-available/fuels-craft`
   - Configure SSL certificate
   - Reload Nginx

4. **Schedule Backups** (recommended)
   - Backup uploaded tree inventories
   - Add cron job for automated backups

---

## Support

**For deployment questions:**
- Review DEPLOYMENT.md
- Check container logs: `fab -H host logs`
- Contact: bdrye@uw.edu

**For UW infrastructure:**
- eScience: escience@uw.edu
- UW-IT: itconnect.uw.edu

---

## What's Different from Manual Deployment

| Aspect | Manual | With These Files |
|--------|--------|------------------|
| Build time | Manual each time | Automated |
| Deployment steps | 10+ commands | 1 command |
| Rollback | Manual image management | One command |
| Monitoring | Manual log checking | Real-time logs |
| Scaling | Manual VM setup | Fabric handles it |
| Documentation | Scattered | All in one place |

---

**You're ready to deploy!** 🚀

Start with:
```bash
./deploy.sh user@your-uw-server.washington.edu deploy
```
