# Quick Start Guide

## Installation

1. **Run setup script**:
   ```bash
   chmod +x setup.sh
   ./setup.sh
   ```

2. **Configure API key**:
   Edit `server/.env` and add your FastFuels API key

3. **Start the application**:

   **Option A: Manual start**
   ```bash
   # Terminal 1 - Backend
   cd server
   npm start

   # Terminal 2 - Frontend
   cd client
   npm start
   ```

   **Option B: Docker**
   ```bash
   docker-compose up
   ```

4. **Access the application**:
   Open http://localhost:3000 in your browser

## First Steps

1. **Draw a polygon** on the map using the polygon tool
2. **Create a domain** by clicking "Create Domain"
3. **Generate tree inventory** using FastFuels API
4. **Customize fuels** using the sidebar controls

## Tips

- Use UTM Zone selector for your area of interest
- TreeMap "fusion" mode uses Meta's canopy height model
- Export tree inventory as CSV for offline editing
- Load custom tree inventories from CSV files

## Need Help?

See the full README.md for detailed documentation.
