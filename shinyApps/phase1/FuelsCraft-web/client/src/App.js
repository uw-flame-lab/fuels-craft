import React, { useState, useCallback, useEffect } from 'react';
import DualMaps from './components/DualMaps';
import Sidebar from './components/Sidebar';
import { fastfuelsAPI, dataAPI } from './services/api';
import proj4 from 'proj4';
import './App.css';

// Helper function to extract tree height
function getTreeHeight(tree) {
  if (tree.HT !== undefined && tree.HT !== null) {
    const ht = parseFloat(tree.HT);
    return Number.isNaN(ht) ? null : ht;
  }
  if (tree.height !== undefined && tree.height !== null) {
    const ht = parseFloat(tree.height);
    return Number.isNaN(ht) ? null : ht;
  }
  return null;
}

// Generate histogram data with height bins
function generateHistogramData(trees) {
  if (!trees || trees.length === 0) {
    return {
      bins: [0, 0, 0, 0, 0, 0, 0],
      labels: ['<5m', '5-10m', '10-15m', '15-20m', '20-30m', '30-50m', '>50m']
    };
  }

  const bins = [0, 0, 0, 0, 0, 0, 0];
  trees.forEach(tree => {
    const height = getTreeHeight(tree);
    if (height !== null) {
      if (height < 5) bins[0]++;
      else if (height < 10) bins[1]++;
      else if (height < 15) bins[2]++;
      else if (height < 20) bins[3]++;
      else if (height < 30) bins[4]++;
      else if (height < 50) bins[5]++;
      else bins[6]++;
    }
  });

  return {
    bins,
    labels: ['<5m', '5-10m', '10-15m', '15-20m', '20-30m', '30-50m', '>50m']
  };
}

// Histogram component - supports single or dual datasets
function Histogram({ title, data, colors, data2, colors2 }) {
  const maxCount = Math.max(
    ...(data2 ? [...data.bins, ...data2.bins] : data.bins),
    1
  );
  const barHeight = 150;

  return (
    <div style={{ padding: '10px', border: '1px solid #ddd', borderRadius: '4px' }}>
      <div style={{ fontSize: '13px', fontWeight: 'bold', marginBottom: '10px' }}>{title}</div>
      <div style={{ display: 'flex', alignItems: 'flex-end', gap: '8px', height: barHeight }}>
        {data.bins.map((count, idx) => (
          <div key={idx} style={{ flex: 1, display: 'flex', alignItems: 'flex-end', gap: '2px', height: '100%', flexDirection: 'row' }}>
            {/* FastFuels bar */}
            <div
              style={{
                flex: 1,
                height: `${(count / maxCount) * barHeight}px`,
                backgroundColor: colors[idx],
                borderRadius: '2px 2px 0 0',
                minHeight: count > 0 ? '2px' : '0px',
                display: 'flex',
                alignItems: 'flex-end',
                justifyContent: 'center',
                fontSize: '8px',
                color: '#fff',
                paddingBottom: '2px'
              }}
            >
              {count > 5 ? count : ''}
            </div>
            {/* Custom bar */}
            {data2 && (
              <div
                style={{
                  flex: 1,
                  height: `${(data2.bins[idx] / maxCount) * barHeight}px`,
                  backgroundColor: colors2[idx],
                  borderRadius: '2px 2px 0 0',
                  minHeight: data2.bins[idx] > 0 ? '2px' : '0px',
                  display: 'flex',
                  alignItems: 'flex-end',
                  justifyContent: 'center',
                  fontSize: '8px',
                  color: '#fff',
                  paddingBottom: '2px'
                }}
              >
                {data2.bins[idx] > 5 ? data2.bins[idx] : ''}
              </div>
            )}
          </div>
        ))}
      </div>
      {/* Labels row */}
      <div style={{ display: 'flex', gap: '8px', marginTop: '8px' }}>
        {data.labels.map((label, idx) => (
          <div key={idx} style={{ flex: 1, fontSize: '10px', textAlign: 'center' }}>
            {label}
          </div>
        ))}
      </div>
      {/* Legend */}
      {data2 && (
        <div style={{ marginTop: '8px', fontSize: '11px', display: 'flex', gap: '20px' }}>
          <div style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
            <div style={{ width: '12px', height: '12px', backgroundColor: colors[0], borderRadius: '1px' }} />
            <span>FastFuels</span>
          </div>
          <div style={{ display: 'flex', alignItems: 'center', gap: '6px' }}>
            <div style={{ width: '12px', height: '12px', backgroundColor: colors2[0], borderRadius: '1px' }} />
            <span>Custom</span>
          </div>
        </div>
      )}
    </div>
  );
}

function App() {
  // State management
  const [polygon, setPolygon] = useState(null);
  const [domainId, setDomainId] = useState('');
  const [apiKey, setApiKey] = useState('88558dfdb7414c73803fca08edb0119e');
  const [utmZone, setUtmZone] = useState('26910');
  const [customTreeFile, setCustomTreeFile] = useState('tree_inventory_test1_customWinthrop.csv');
  const [treeMapVersion, setTreeMapVersion] = useState('2016');
  const [treeMapStyle, setTreeMapStyle] = useState('fusion');
  const [qfInputFormat, setQfInputFormat] = useState('zip');
  const [loading, setLoading] = useState(false);
  const [messages, setMessages] = useState({});

  const [ffTreeData, setFfTreeData] = useState(null);
  const [customTreeData, setCustomTreeData] = useState(null);
  const [showFFTrees, setShowFFTrees] = useState(true);
  const [showCustomTrees, setShowCustomTrees] = useState(true);

  const updateMessage = (key, value) => {
    setMessages(prev => ({ ...prev, [key]: value }));
  };

  // Debug: log when tree data changes
  useEffect(() => {
    console.log('App: ffTreeData changed:', ffTreeData);
  }, [ffTreeData]);

  const handlePolygonCreated = useCallback((geojson) => {
    setPolygon(geojson);

    // Calculate area
    const coords = geojson.geometry.coordinates[0];
    const area = calculatePolygonArea(coords);
    updateMessage('area', `Area: ${area.toFixed(2)} m²`);
  }, []);

  const handlePolygonEdited = useCallback((geojson) => {
    setPolygon(geojson);
  }, []);

  const handlePolygonDeleted = useCallback(() => {
    setPolygon(null);
    updateMessage('area', '');
  }, []);

  const handleClearMap = () => {
    setPolygon(null);
    setDomainId('');
    setMessages({});
    setFfTreeData(null);
    setCustomTreeData(null);
  };

  const handleCreateDomain = async () => {
    if (!polygon) {
      alert('Please draw a polygon first');
      return;
    }

    setLoading(true);
    try {
      const result = await fastfuelsAPI.createDomain(polygon, apiKey);

      if (result.error) {
        updateMessage('area', result.error);
      } else {
        setDomainId(result.id);
        updateMessage('area', `Domain created: ${result.id}`);
      }
    } catch (error) {
      alert('Error creating domain: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateRoadFeature = async () => {
    setLoading(true);
    try {
      await fastfuelsAPI.createRoadFeature(domainId, apiKey);
      updateMessage('roadFeature', 'completed');
    } catch (error) {
      alert('Error creating road feature: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateWaterFeature = async () => {
    setLoading(true);
    try {
      await fastfuelsAPI.createWaterFeature(domainId, apiKey);
      updateMessage('waterFeature', 'completed');
    } catch (error) {
      alert('Error creating water feature: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateTreeInventory = async () => {
    setLoading(true);
    try {
      await fastfuelsAPI.createTreeInventory(domainId, apiKey, {
        treeMapVersion,
        treeMapStyle
      });
      updateMessage('treeInventory', 'completed');
    } catch (error) {
      alert('Error creating tree inventory: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleExportTreeInventory = async () => {
    setLoading(true);
    try {
      const result = await fastfuelsAPI.exportTreeInventory(domainId, apiKey);
      console.log('Export result:', result);
      updateMessage('exportUrl', result.signedUrl);

      // Load the CSV data
      console.log('Fetching CSV from:', `/api/fastfuels/proxy-csv?url=${encodeURIComponent(result.signedUrl.substring(0, 50))}...`);
      const response = await fetch(`/api/fastfuels/proxy-csv?url=${encodeURIComponent(result.signedUrl)}`);
      console.log('CSV response status:', response.status, response.statusText);

      if (!response.ok) {
        throw new Error(`Failed to fetch CSV: ${response.statusText}`);
      }

      const csvText = await response.text();
      console.log('CSV text length:', csvText.length);
      console.log('CSV first 500 chars:', csvText.substring(0, 500));

      // Parse CSV data
      const lines = csvText.trim().split('\n').filter(line => line.trim().length > 0);
      console.log('Number of lines (including header):', lines.length);

      if (lines.length < 2) {
        console.warn('CSV has no data rows');
        updateMessage('treeInventory', 'No tree data found');
        setLoading(false);
        return;
      }

      const headers = lines[0].split(',').map(h => h.trim());
      console.log('CSV Headers:', headers);

      const trees = lines.slice(1)
        .filter(line => line.trim().length > 0)
        .map((line, lineIndex) => {
          const values = line.split(',').map(v => v.trim());
          const tree = {};
          headers.forEach((header, index) => {
            tree[header] = values[index];
          });
          if (lineIndex < 3) {
            console.log(`Tree ${lineIndex}:`, tree);
          }
          return tree;
        });

      console.log('Trees loaded:', trees.length, 'Sample:', trees[0]);
      console.log('Setting ffTreeData state:', trees);
      setFfTreeData(trees);
      updateMessage('treeInventory', `Tree inventory loaded: ${trees.length} trees`);
    } catch (error) {
      console.error('Error exporting tree inventory:', error);
      alert('Error exporting tree inventory: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleLoadCustomFile = async (file) => {
    if (!file && !customTreeFile) {
      alert('Please select a file');
      return;
    }

    setLoading(true);
    try {
      if (file) {
        const result = await dataAPI.loadCSV(file);
        setCustomTreeData(result.data);
        updateMessage('area', `Loaded ${result.data.length} trees from file`);
      } else {
        // Load from hardcoded file path
        updateMessage('area', `Loading ${customTreeFile}...`);
        const response = await fetch(`/${customTreeFile}`);
        const csvText = await response.text();
        const trees = parseCSV(csvText);
        setCustomTreeData(trees);
        updateMessage('area', `Loaded ${trees.length} trees from ${customTreeFile}`);
      }
    } catch (error) {
      alert('Error loading file: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateTreeGrid = async () => {
    if (!domainId) {
      alert('Please create a domain first');
      return;
    }
    setLoading(true);
    try {
      await fastfuelsAPI.createTreeGrid(domainId, apiKey);
      updateMessage('treeGrid', 'completed');
    } catch (error) {
      alert('Error creating tree grid: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateSurfaceGrid = async () => {
    if (!domainId) {
      alert('Please create a domain first');
      return;
    }
    setLoading(true);
    try {
      await fastfuelsAPI.createSurfaceGrid(domainId, apiKey);
      updateMessage('surfaceGrid', 'completed');
    } catch (error) {
      alert('Error creating surface grid: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleCreateTopographyGrid = async () => {
    if (!domainId) {
      alert('Please create a domain first');
      return;
    }
    setLoading(true);
    try {
      await fastfuelsAPI.createTopographyGrid(domainId, apiKey);
      updateMessage('topographyGrid', 'completed');
    } catch (error) {
      alert('Error creating topography grid: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleGetInputFiles = async () => {
    if (!domainId) {
      alert('Please create a domain first');
      return;
    }
    setLoading(true);
    try {
      const result = await fastfuelsAPI.getInputFiles(domainId, apiKey, qfInputFormat);
      updateMessage('inputFiles', result.signedUrl || 'Files ready');
      if (result.signedUrl) {
        // Auto-download or show link
        window.open(result.signedUrl, '_blank');
      }
    } catch (error) {
      alert('Error getting input files: ' + error.message);
    } finally {
      setLoading(false);
    }
  };

  const handleMergeCustomToFF = () => {
    if (!customTreeData || customTreeData.length === 0) {
      alert('No custom trees to merge');
      return;
    }

    // Merge custom trees into FastFuels trees
    const mergedTrees = [...(ffTreeData || []), ...customTreeData];
    setFfTreeData(mergedTrees);
    setCustomTreeData([]);

    updateMessage('merge', `Merged ${customTreeData.length} custom trees into FastFuels`);
  };

  // Helper function to check if a point is inside a polygon (ray casting algorithm)
  const isPointInPolygon = (point, polygonCoords) => {
    const [x, y] = point;
    let inside = false;

    for (let i = 0, j = polygonCoords.length - 1; i < polygonCoords.length; j = i++) {
      const [xi, yi] = polygonCoords[i];
      const [xj, yj] = polygonCoords[j];

      const intersect = ((yi > y) !== (yj > y)) && (x < ((xj - xi) * (y - yi)) / (yj - yi) + xi);
      if (intersect) inside = !inside;
    }

    return inside;
  };

  // Helper function to convert tree to [lon, lat] format (GeoJSON standard)
  const getTreeCoords = (tree) => {
    // Try different coordinate field names
    if (tree.latitude !== undefined && tree.longitude !== undefined) {
      const lat = parseFloat(tree.latitude);
      const lon = parseFloat(tree.longitude);
      if (!Number.isNaN(lat) && !Number.isNaN(lon)) {
        return [lon, lat];
      }
    }
    if (tree.lat !== undefined && tree.lon !== undefined) {
      const lat = parseFloat(tree.lat);
      const lon = parseFloat(tree.lon);
      if (!Number.isNaN(lat) && !Number.isNaN(lon)) {
        return [lon, lat];
      }
    }
    if (tree.y !== undefined && tree.x !== undefined) {
      const lat = parseFloat(tree.y);
      const lon = parseFloat(tree.x);
      if (!Number.isNaN(lat) && !Number.isNaN(lon)) {
        return [lon, lat];
      }
    }
    // Handle UTM coordinates (X, Y fields)
    if (tree.Y !== undefined && tree.X !== undefined) {
      try {
        const sourceProjection = getUtmProjection(utmZone);
        if (sourceProjection) {
          const easting = parseFloat(tree.X);
          const northing = parseFloat(tree.Y);
          if (!Number.isNaN(easting) && !Number.isNaN(northing)) {
            const [lon, lat] = proj4(sourceProjection, 'EPSG:4326', [easting, northing]);
            return [lon, lat];
          }
        }
      } catch (e) {
        console.error('Error converting UTM coordinates:', e);
      }
    }
    return null;
  };

  // Helper function to get UTM projection string
  const getUtmProjection = (utmZone) => {
    const epsg = String(utmZone || '26910');

    if (/^269\d{2}$/.test(epsg)) {
      const zone = parseInt(epsg.slice(-2), 10);
      return `+proj=utm +zone=${zone} +datum=NAD83 +units=m +no_defs`;
    }

    if (/^326\d{2}$/.test(epsg)) {
      const zone = parseInt(epsg.slice(-2), 10);
      return `+proj=utm +zone=${zone} +datum=WGS84 +units=m +no_defs`;
    }

    if (/^327\d{2}$/.test(epsg)) {
      const zone = parseInt(epsg.slice(-2), 10);
      return `+proj=utm +zone=${zone} +south +datum=WGS84 +units=m +no_defs`;
    }

    const fallbackZone = parseInt(epsg.slice(-2), 10);
    if (!Number.isNaN(fallbackZone)) {
      return `+proj=utm +zone=${fallbackZone} +datum=NAD83 +units=m +no_defs`;
    }

    return null;
  };

  // Remove trees inside the drawn polygon
  const handleRemoveTreesInPolygon = () => {
    if (!polygon) {
      alert('Please draw a polygon first');
      return;
    }

    if (!ffTreeData || ffTreeData.length === 0) {
      alert('No FastFuels trees to remove');
      return;
    }

    const polygonCoords = polygon.geometry.coordinates[0];
    const treesOutside = ffTreeData.filter(tree => {
      const coords = getTreeCoords(tree);
      if (!coords || Number.isNaN(coords[0]) || Number.isNaN(coords[1])) {
        return true; // Keep trees with invalid coordinates
      }
      return !isPointInPolygon(coords, polygonCoords);
    });

    const removedCount = ffTreeData.length - treesOutside.length;
    setFfTreeData(treesOutside);
    updateMessage('removeTrees', `Removed ${removedCount} trees from FastFuels`);
  };

  // Add trees randomly in the drawn polygon
  const handleAddTreesInPolygon = () => {
    if (!polygon) {
      alert('Please draw a polygon first');
      return;
    }

    if (!ffTreeData || ffTreeData.length === 0) {
      alert('No FastFuels trees to sample from');
      return;
    }

    const numTrees = prompt('How many trees to add?', '10');
    if (!numTrees || isNaN(parseInt(numTrees))) {
      return;
    }

    const count = parseInt(numTrees);
    const polygonCoords = polygon.geometry.coordinates[0];

    // Get bounding box of polygon
    const lats = polygonCoords.map(c => c[1]);
    const lons = polygonCoords.map(c => c[0]);
    const minLat = Math.min(...lats);
    const maxLat = Math.max(...lats);
    const minLon = Math.min(...lons);
    const maxLon = Math.max(...lons);

    // Generate random points in polygon
    const newTrees = [];
    let attempts = 0;
    const maxAttempts = count * 10; // Prevent infinite loop

    while (newTrees.length < count && attempts < maxAttempts) {
      const randomLat = minLat + Math.random() * (maxLat - minLat);
      const randomLon = minLon + Math.random() * (maxLon - minLon);

      if (isPointInPolygon([randomLon, randomLat], polygonCoords)) {
        // Sample a random tree from ffTreeData
        const sampleTree = { ...ffTreeData[Math.floor(Math.random() * ffTreeData.length)] };

        // Update coordinates - set both lat/lon and latitude/longitude for compatibility
        sampleTree.latitude = randomLat;
        sampleTree.longitude = randomLon;
        sampleTree.lat = randomLat;
        sampleTree.lon = randomLon;

        newTrees.push(sampleTree);
      }

      attempts++;
    }

    if (newTrees.length > 0) {
      const updatedTrees = [...ffTreeData, ...newTrees];
      setFfTreeData(updatedTrees);
      updateMessage('addTrees', `Added ${newTrees.length} trees to FastFuels`);
    }
  };

  return (
    <div style={{ display: 'flex', height: '100vh' }}>
      <Sidebar
        onCreateDomain={handleCreateDomain}
        onCreateRoadFeature={handleCreateRoadFeature}
        onCreateWaterFeature={handleCreateWaterFeature}
        onCreateTreeInventory={handleCreateTreeInventory}
        onExportTreeInventory={handleExportTreeInventory}
        onCreateTreeGrid={handleCreateTreeGrid}
        onCreateSurfaceGrid={handleCreateSurfaceGrid}
        onCreateTopographyGrid={handleCreateTopographyGrid}
        onGetInputFiles={handleGetInputFiles}
        onLoadCustomFile={handleLoadCustomFile}
        onClearMap={handleClearMap}
        domainId={domainId}
        setDomainId={setDomainId}
        apiKey={apiKey}
        setApiKey={setApiKey}
        treeMapVersion={treeMapVersion}
        setTreeMapVersion={setTreeMapVersion}
        treeMapStyle={treeMapStyle}
        setTreeMapStyle={setTreeMapStyle}
        qfInputFormat={qfInputFormat}
        setQfInputFormat={setQfInputFormat}
        utmZone={utmZone}
        setUtmZone={setUtmZone}
        customTreeFile={customTreeFile}
        setCustomTreeFile={setCustomTreeFile}
        loading={loading}
        messages={messages}
      />

      <div style={{ flex: 1, padding: '20px' }}>
        <DualMaps
          onPolygonCreated={handlePolygonCreated}
          onPolygonEdited={handlePolygonEdited}
          onPolygonDeleted={handlePolygonDeleted}
          treeData={{ ff: ffTreeData, custom: customTreeData }}
          showFFTrees={showFFTrees}
          showCustomTrees={showCustomTrees}
          utmZone={utmZone}
        />

        {/* Tree inventory controls would go here */}
        <div style={{ marginTop: '20px' }}>
          <div style={{ display: 'flex', gap: '30px', alignItems: 'center' }}>
            <label style={{ display: 'flex', alignItems: 'center', gap: '8px', margin: 0 }}>
              <input
                type="checkbox"
                checked={showFFTrees}
                onChange={(e) => setShowFFTrees(e.target.checked)}
              />
              Show FastFuels Trees
            </label>

            <label style={{ display: 'flex', alignItems: 'center', gap: '8px', margin: 0 }}>
              <input
                type="checkbox"
                checked={showCustomTrees}
                onChange={(e) => setShowCustomTrees(e.target.checked)}
              />
              Show Custom Trees
            </label>
          </div>

          {/* Tree editing controls */}
          {polygon && ffTreeData && ffTreeData.length > 0 && (
            <div style={{ marginTop: '15px', display: 'flex', gap: '10px' }}>
              <button
                onClick={handleRemoveTreesInPolygon}
                style={{
                  padding: '8px 16px',
                  backgroundColor: '#ef4444',
                  color: 'white',
                  border: 'none',
                  borderRadius: '4px',
                  cursor: 'pointer',
                  fontSize: '14px',
                  fontWeight: '500'
                }}
              >
                Remove Trees
              </button>
              <button
                onClick={handleAddTreesInPolygon}
                style={{
                  padding: '8px 16px',
                  backgroundColor: '#3b82f6',
                  color: 'white',
                  border: 'none',
                  borderRadius: '4px',
                  cursor: 'pointer',
                  fontSize: '14px',
                  fontWeight: '500'
                }}
              >
                Add Trees
              </button>
            </div>
          )}

          {/* Combined Height Distribution Histogram */}
          <div style={{ marginTop: '15px' }}>
            <Histogram
              title="Height Distribution Comparison"
              data={generateHistogramData(ffTreeData)}
              colors={['#e0f2fe', '#bae6fd', '#7dd3fc', '#38bdf8', '#0ea5e9', '#0284c7', '#075985']}
              data2={generateHistogramData(customTreeData)}
              colors2={['#fef3c7', '#fed7aa', '#fdba74', '#fb923c', '#f97316', '#ea580c', '#9a3412']}
            />

            {/* Merge button */}
            {customTreeData && customTreeData.length > 0 && (
              <button
                onClick={handleMergeCustomToFF}
                style={{
                  marginTop: '10px',
                  padding: '8px 16px',
                  backgroundColor: '#10b981',
                  color: 'white',
                  border: 'none',
                  borderRadius: '4px',
                  cursor: 'pointer',
                  fontSize: '14px',
                  fontWeight: '500'
                }}
              >
                Add Custom to FastFuels
              </button>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

// Helper function to parse CSV data
function parseCSV(csvText) {
  const lines = csvText.trim().split('\n');
  if (lines.length < 2) return [];

  // Parse header (first line)
  const headerLine = lines[0];
  const headers = headerLine.split(',').map(h => h.trim().replace(/^"|"$/g, ''));

  // Parse data rows
  const trees = [];
  for (let i = 1; i < lines.length; i++) {
    const line = lines[i];
    const values = line.split(',').map(v => v.trim().replace(/^"|"$/g, ''));

    if (values.length === headers.length) {
      const tree = {};
      headers.forEach((header, index) => {
        tree[header] = values[index] === 'NA' ? undefined : values[index];
      });
      trees.push(tree);
    }
  }

  return trees;
}

// Helper function to calculate polygon area
function calculatePolygonArea(coordinates) {
  // Simple area calculation (not geodesic)
  let area = 0;
  for (let i = 0; i < coordinates.length - 1; i++) {
    const [x1, y1] = coordinates[i];
    const [x2, y2] = coordinates[i + 1];
    area += x1 * y2 - x2 * y1;
  }
  return Math.abs(area / 2);
}

export default App;
