# FuelsCraft Web Application

A modern web-based fuel customization tool converted from the original Shiny application. This application provides an interface for managing fuel inventories, integrating with FastFuels API, and customizing understory fuel characteristics.

## Features

- **Dual Synchronized Maps**: Interactive Leaflet maps with polygon drawing capabilities
- **FastFuels API Integration**: Create domains, manage tree inventories, generate grids
- **Tree Inventory Management**: Load, filter, add/remove trees, merge inventories
- **Understory Fuel Customization**: Configure shrubs, herbs, downed wood, litter, and ground fuels
- **File Operations**: Upload/download CSV files, export tree inventories
- **Real-time Updates**: Live map updates and data processing

## Architecture

### Backend (Node.js/Express)
- REST API for FastFuels proxy
- File upload/download endpoints
- CSV processing and data transformations
- Session management

### Frontend (React)
- React components with Leaflet maps
- Polygon drawing with Leaflet.Draw
- State management for tree inventories
- Responsive UI with real-time updates

## Installation

### Prerequisites
- Node.js 16+ and npm
- FastFuels API key

### Backend Setup

```bash
cd server
npm install
cp .env.example .env
# Edit .env and add your FastFuels API key
npm start
```

The server will run on `http://localhost:3001`

### Frontend Setup

```bash
cd client
npm install
npm start
```

The client will run on `http://localhost:3000`

## Usage

1. **Draw a Polygon**: Use the polygon tool on the main map to define your area of interest
2. **Create Domain**: Click "Create Domain" to register your polygon with FastFuels API
3. **Configure Features**: Add road and water features from OpenStreetMap
4. **Generate Tree Inventory**: Create tree inventory using TreeMap data
5. **Customize**: Load custom tree inventories, filter by height, add/remove trees
6. **Export**: Download tree inventory as CSV or generate QUIC-Fire input files

## API Endpoints

### FastFuels Proxy
- `POST /api/fastfuels/domains` - Create new domain
- `POST /api/fastfuels/domains/:id/features/road` - Create road features
- `POST /api/fastfuels/domains/:id/features/water` - Create water features
- `POST /api/fastfuels/domains/:id/inventories/tree` - Generate tree inventory
- `POST /api/fastfuels/domains/:id/grids/tree` - Create tree grid
- `POST /api/fastfuels/domains/:id/grids/surface` - Create surface grid
- `POST /api/fastfuels/domains/:id/grids/topography` - Create topography grid
- `POST /api/fastfuels/domains/:id/grids/exports/:format` - Export grids (zip/zarr)

### Data Management
- `POST /api/data/load-csv` - Upload and parse CSV file
- `POST /api/data/save-inventory` - Save tree inventory to CSV
- `POST /api/data/process-inventory` - Process inventory operations
- `GET /api/data/fuelbeds` - Get available fuelbed definitions

## Development

### Project Structure

```
FuelsCraft-web/
├── server/
│   ├── index.js           # Express server
│   ├── routes/
│   │   ├── fastfuels.js   # FastFuels API proxy
│   │   └── data.js        # Data processing endpoints
│   └── package.json
├── client/
│   ├── src/
│   │   ├── App.js         # Main application component
│   │   ├── components/
│   │   │   ├── DualMaps.js     # Synchronized map components
│   │   │   └── Sidebar.js      # Control panel
│   │   └── services/
│   │       └── api.js     # API client
│   └── package.json
└── README.md
```

### Adding Features

To add new features:
1. Add backend route in `server/routes/`
2. Add API method in `client/src/services/api.js`
3. Create/update React component
4. Wire up in `App.js`

## Differences from Shiny App

| Feature | Shiny | Web App |
|---------|-------|---------|
| State Management | Reactive values | React state |
| Map Updates | leafletProxy | Direct Leaflet API |
| API Calls | httr::POST/GET | axios |
| File Upload | fileInput | FormData |
| Interactivity | Shiny observers | Event handlers |

## Deployment

### Docker (Recommended)

Create `docker-compose.yml`:

```yaml
version: '3.8'
services:
  server:
    build: ./server
    ports:
      - "3001:3001"
    environment:
      - PORT=3001
      - FASTFUELS_API_URL=https://api.fastfuels.silvxlabs.com/v1

  client:
    build: ./client
    ports:
      - "3000:3000"
    depends_on:
      - server
```

Run: `docker-compose up`

### Traditional Deployment

1. Build frontend: `cd client && npm run build`
2. Serve static files from Express: Add `app.use(express.static('../client/build'))` in `server/index.js`
3. Deploy to any Node.js hosting (Heroku, AWS, DigitalOcean, etc.)

## Configuration

### Environment Variables

**Server (.env)**:
- `PORT` - Server port (default: 3001)
- `FASTFUELS_API_URL` - FastFuels API base URL
- `DEFAULT_API_KEY` - Default API key (optional)

**Client**:
- `REACT_APP_API_URL` - Backend API URL (default: http://localhost:3001/api)

## Troubleshooting

**Maps not syncing**: Ensure Leaflet.Sync script is loaded in index.html

**CORS errors**: Check server CORS configuration matches client origin

**API key errors**: Verify API key is valid and not expired

**File upload fails**: Check multer configuration and file size limits

## Future Enhancements

- [ ] Complete understory fuel customization UI
- [ ] Add histogram charts for tree heights
- [ ] Implement point-in-polygon operations for tree filtering
- [ ] Add CHM raster processing
- [ ] LANDFIRE integration
- [ ] User authentication and session persistence
- [ ] WebSocket support for real-time progress updates
- [ ] Mobile responsive design

## License

Same as original Shiny application

## Credits

Converted from FuelsCraft Shiny application by UW Flame Lab
