# Shiny to Web App Migration Guide

This document maps the original Shiny app functionality to the new web application architecture.

## Architecture Comparison

| Component | Shiny App | Web App |
|-----------|-----------|---------|
| **Backend** | R/Shiny server | Node.js/Express |
| **Frontend** | Shiny UI + reactive HTML | React components |
| **State** | reactiveValues() | React useState/useCallback |
| **Maps** | leaflet + leafletProxy | react-leaflet + Leaflet API |
| **API Calls** | httr (POST/GET) | axios |
| **File Upload** | fileInput + read.csv | multer + papaparse |
| **Sessions** | Shiny session | express-session |

## Feature Mapping

### Maps & Polygon Drawing

**Shiny (`ui.R`)**:
```r
leafletOutput("map")
addDrawToolbar(...)
observeEvent(input$map_draw_new_feature, { ... })
```

**Web App**:
```javascript
<DualMaps
  onPolygonCreated={handlePolygonCreated}
  onPolygonEdited={handlePolygonEdited}
/>
```

### FastFuels API Integration

**Shiny (`server.R`)**:
```r
observeEvent(input$createDomain, {
  headers <- add_headers(`api-key` = input$api_key)
  response <- POST(url, headers, body = geojson)
  contentResponse <- content(response, "parsed")
})
```

**Web App**:
```javascript
// Backend (routes/fastfuels.js)
router.post('/domains', async (req, res) => {
  const response = await axios.post(url, geojson, { headers });
  res.json(response.data);
});

// Frontend (App.js)
const result = await fastfuelsAPI.createDomain(polygon, apiKey);
```

### Tree Inventory Management

**Shiny**:
```r
rv$ff_data <- read.csv(source)
rv$ff_data2 <- rv$ff_data[filter_condition, ]
updateSliderInput(session, "ffTreeHeight", ...)
```

**Web App**:
```javascript
const [ffTreeData, setFfTreeData] = useState(null);
const filtered = ffTreeData.filter(tree =>
  tree.HT >= minHeight && tree.HT <= maxHeight
);
```

### File Operations

**Shiny**:
```r
write.csv(rv$ff_data_temp, filename, row.names = FALSE)
input$customTreeFile  # fileInput
```

**Web App**:
```javascript
// Backend
router.post('/save-inventory', async (req, res) => {
  const csv = Papa.unparse(data);
  await fs.writeFile(filepath, csv);
});

// Frontend
const file = fileInput.files[0];
await dataAPI.loadCSV(file);
```

## Key Differences

### 1. Reactive Programming vs Event-Driven

**Shiny**: Automatic reactivity
```r
observe({
  if (input$showFFTrees) {
    # Automatically runs when input changes
  }
})
```

**Web App**: Explicit event handlers
```javascript
const handleShowTreesChange = (checked) => {
  setShowFFTrees(checked);
  // Manually trigger updates
};
```

### 2. Server-Side vs Client-Side State

**Shiny**: State lives on the server
```r
rv <- reactiveValues()
rv$polygon <- NULL
rv$ff_data <- NULL
```

**Web App**: State lives in the browser
```javascript
const [polygon, setPolygon] = useState(null);
const [ffTreeData, setFfTreeData] = useState(null);
```

### 3. Map Updates

**Shiny**: Server-side proxy
```r
proxy <- leafletProxy("map")
proxy %>% clearGroup("ff_circles")
proxy %>% addCircles(data = filtered_data, ...)
```

**Web App**: Direct client-side updates
```javascript
const map = useMap();
map.eachLayer(layer => {
  if (layer.options.group === 'ff_circles') {
    map.removeLayer(layer);
  }
});
```

### 4. Async Operations

**Shiny**: Synchronous with polling
```r
response <- POST(url, ...)
showPageSpinner()
while (contentResponse$status != "completed") {
  Sys.sleep(5)
  response <- GET(url, ...)
}
hidePageSpinner()
```

**Web App**: Native async/await
```javascript
setLoading(true);
const response = await axios.post(url, data);
const result = await pollForCompletion(url, apiKey);
setLoading(false);
```

## Migration Checklist

### Completed ✅
- [x] Project structure (backend + frontend)
- [x] Express server with routes
- [x] React app with components
- [x] FastFuels API proxy
- [x] Dual synchronized maps
- [x] Polygon drawing tools
- [x] File upload/download
- [x] Basic tree inventory management
- [x] Docker configuration
- [x] Documentation

### To Be Implemented 🔨
- [ ] Tree filtering by polygon (point-in-polygon)
- [ ] Histogram charts (Chart.js integration)
- [ ] Understory fuel customization UI
- [ ] CHM raster processing
- [ ] LANDFIRE API integration
- [ ] Species database integration
- [ ] Fuelbed XML parsing and display
- [ ] Advanced tree manipulation (add/remove by polygon)
- [ ] Coordinate transformation utilities
- [ ] Export to multiple formats
- [ ] Session persistence
- [ ] WebSocket for progress updates

## Development Workflow

### Adding a New Feature

1. **Backend Route** (`server/routes/`)
```javascript
router.post('/new-feature', async (req, res) => {
  // Handle request
  res.json(result);
});
```

2. **API Client** (`client/src/services/api.js`)
```javascript
export const dataAPI = {
  newFeature: async (params) => {
    const response = await axios.post(`${API_BASE_URL}/new-feature`, params);
    return response.data;
  }
};
```

3. **React Component** (`client/src/components/NewFeature.js`)
```javascript
function NewFeature() {
  const handleAction = async () => {
    const result = await dataAPI.newFeature(params);
    // Update UI
  };
  return <button onClick={handleAction}>Action</button>;
}
```

4. **Wire Up** (`client/src/App.js`)
```javascript
import NewFeature from './components/NewFeature';
// Add to render
<NewFeature onAction={handleAction} />
```

## Testing

### Backend
```bash
cd server
npm test  # (add Jest tests)
```

### Frontend
```bash
cd client
npm test  # (React Testing Library)
```

### Integration
- Use Postman/Insomnia for API testing
- Browser DevTools for frontend debugging
- Network tab for API call inspection

## Deployment Considerations

### Environment Variables
- **Development**: Local `.env` files
- **Production**: Platform-specific (Heroku Config Vars, AWS Secrets Manager, etc.)

### Build Process
```bash
# Frontend production build
cd client && npm run build

# Serve from Express
app.use(express.static('../client/build'));
```

### Scaling
- Use Redis for session storage
- Add load balancer for multiple instances
- CDN for static assets
- Database for persistent storage

## Common Issues

### CORS Errors
Solution: Configure CORS in `server/index.js`
```javascript
app.use(cors({ origin: 'http://your-frontend-url' }));
```

### Map Not Syncing
Solution: Ensure Leaflet.Sync script loads before React app

### API Key Exposure
Solution: Never commit `.env` files, use backend proxy for API calls

### Large File Uploads
Solution: Increase multer limits and use streaming for large files

## Performance Tips

1. **Memoization**: Use `React.memo()` for expensive components
2. **Lazy Loading**: Split code with `React.lazy()`
3. **Map Optimization**: Cluster markers for large datasets
4. **Debouncing**: Debounce slider inputs to reduce updates
5. **Caching**: Cache API responses where appropriate

## Resources

- [React Documentation](https://react.dev/)
- [Leaflet Documentation](https://leafletjs.com/)
- [Express.js Guide](https://expressjs.com/)
- [FastFuels API Docs](https://api.fastfuels.silvxlabs.com/)
