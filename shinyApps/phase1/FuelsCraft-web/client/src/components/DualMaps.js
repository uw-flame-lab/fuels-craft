import React, { useEffect, useRef } from 'react';
import { MapContainer, TileLayer, useMap } from 'react-leaflet';
import { FeatureGroup } from 'react-leaflet';
import { EditControl } from 'react-leaflet-draw';
import L from 'leaflet';
import proj4 from 'proj4';
import 'leaflet/dist/leaflet.css';
import 'leaflet-draw/dist/leaflet.draw.css';
import 'leaflet.sync'; // Import the sync plugin

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

// Map tree height to color using bins - Blue ramp for FastFuels trees
function getFFTreeHeightColor(height) {
  if (height === null || height === undefined) return '#94a3b8'; // slate for unknown

  if (height < 5) return '#e0f2fe'; // light blue
  if (height < 10) return '#bae6fd'; // sky blue
  if (height < 15) return '#7dd3fc'; // cyan
  if (height < 20) return '#38bdf8'; // blue
  if (height < 30) return '#0ea5e9'; // bright blue
  if (height < 50) return '#0284c7'; // deep blue
  return '#075985'; // navy for very tall
}

// Map tree height to color using bins - Orange/Red ramp for Custom trees
function getCustomTreeHeightColor(height) {
  if (height === null || height === undefined) return '#9ca3af'; // gray for unknown

  if (height < 5) return '#fef3c7'; // light yellow
  if (height < 10) return '#fed7aa'; // light orange
  if (height < 15) return '#fdba74'; // orange
  if (height < 20) return '#fb923c'; // bright orange
  if (height < 30) return '#f97316'; // deep orange
  if (height < 50) return '#ea580c'; // dark orange
  return '#9a3412'; // very dark orange for very tall
}

function getUtmProjection(utmZone) {
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
}

function convertTreeToLatLon(tree, utmZone) {
  if (tree.latitude !== undefined && tree.longitude !== undefined) {
    return [parseFloat(tree.latitude), parseFloat(tree.longitude)];
  }

  if (tree.lat !== undefined && tree.lon !== undefined) {
    return [parseFloat(tree.lat), parseFloat(tree.lon)];
  }

  if (tree.y !== undefined && tree.x !== undefined) {
    return [parseFloat(tree.y), parseFloat(tree.x)];
  }

  if (tree.Y !== undefined && tree.X !== undefined) {
    const sourceProjection = getUtmProjection(utmZone);
    if (!sourceProjection) {
      return [NaN, NaN];
    }

    const easting = parseFloat(tree.X);
    const northing = parseFloat(tree.Y);
    if (Number.isNaN(easting) || Number.isNaN(northing)) {
      return [NaN, NaN];
    }

    const [lon, lat] = proj4(sourceProjection, 'EPSG:4326', [easting, northing]);
    return [lat, lon];
  }

  return [NaN, NaN];
}

// Component to add markers using Leaflet directly (avoids react-leaflet rendering issues)
function TreeMarkersLayer({ treeData, showFFTrees, utmZone }) {
  const map = useMap();
  const layerGroupRef = useRef(null);

  useEffect(() => {
    if (!map || !treeData?.ff || treeData.ff.length === 0) {
      console.log('TreeMarkersLayer: no data or map not ready');
      if (layerGroupRef.current && map) {
        map.removeLayer(layerGroupRef.current);
        layerGroupRef.current = null;
      }
      return;
    }

    if (!showFFTrees) {
      console.log('TreeMarkersLayer: hiding markers');
      if (layerGroupRef.current && map) {
        map.removeLayer(layerGroupRef.current);
      }
      return;
    }

    console.log('TreeMarkersLayer: adding markers to map');

    // Create a layer group for markers
    if (!layerGroupRef.current) {
      layerGroupRef.current = L.layerGroup().addTo(map);
    } else {
      layerGroupRef.current.clearLayers();
      // Re-add to map if it was removed
      if (!map.hasLayer(layerGroupRef.current)) {
        map.addLayer(layerGroupRef.current);
      }
    }

    const treesToRender = treeData.ff;
    let addedCount = 0;

    treesToRender.forEach((tree, index) => {
      const [lat, lon] = convertTreeToLatLon(tree, utmZone);

      if (isNaN(lat) || isNaN(lon)) {
        return;
      }

      const height = getTreeHeight(tree);
      const heightColor = getFFTreeHeightColor(height);
      const dbh = tree.DIA ? parseFloat(tree.DIA) : (tree.dbh ? parseFloat(tree.dbh) : 'N/A');

      // Use circleMarker instead of icon marker for height-colored dots
      L.circleMarker([lat, lon], {
        radius: 4,
        fillColor: heightColor,
        color: '#111827',
        weight: 0.5,
        opacity: 1,
        fillOpacity: 0.95,
        zIndexOffset: 1000
      })
        .bindPopup(`
          <div>
            <strong>Tree ${tree.TREE_ID || index + 1}</strong>
            ${height !== null ? `<div>Height: ${height.toFixed(2)} m</div>` : ''}
            ${dbh !== 'N/A' ? `<div>DBH: ${parseFloat(dbh).toFixed(2)} cm</div>` : ''}
            <div style="font-size: 0.85em; color: #666;">Color: ${heightColor}</div>
          </div>
        `)
        .addTo(layerGroupRef.current);

      addedCount++;
    });

    console.log('TreeMarkersLayer: added', addedCount, 'markers to map');
    if (addedCount > 0) {
      const [sampleLat, sampleLon] = convertTreeToLatLon(treesToRender[0], utmZone);
      console.log('TreeMarkersLayer sample converted:', { sampleLat, sampleLon, utmZone });
    }

    // Cleanup
    return () => {
      // Don't remove on unmount - we want to keep the markers
    };
  }, [map, treeData, showFFTrees, utmZone]);

  return null;
}

// Separate component for tree markers to avoid re-rendering issues
function TreeMarkers({ treeData, showFFTrees, utmZone }) {
  console.log('TreeMarkers render called with:', { treeData: !!treeData, ff: treeData?.ff?.length, showFFTrees });

  if (!treeData?.ff || treeData.ff.length === 0) {
    console.log('TreeMarkers: no data to render');
    return null;
  }

  return <TreeMarkersLayer treeData={treeData} showFFTrees={showFFTrees} utmZone={utmZone} />;
}

// Component to render custom tree markers on left map using Leaflet directly
function CustomTreeMarkersLayer({ treeData, showCustomTrees, utmZone }) {
  const map = useMap();
  const layerGroupRef = useRef(null);

  useEffect(() => {
    if (!map || !treeData?.custom || treeData.custom.length === 0) {
      console.log('CustomTreeMarkersLayer: no data or map not ready');
      if (layerGroupRef.current && map) {
        map.removeLayer(layerGroupRef.current);
        layerGroupRef.current = null;
      }
      return;
    }

    if (!showCustomTrees) {
      console.log('CustomTreeMarkersLayer: hiding markers');
      if (layerGroupRef.current && map) {
        map.removeLayer(layerGroupRef.current);
      }
      return;
    }

    console.log('CustomTreeMarkersLayer: adding markers to map');

    // Create a layer group for markers
    if (!layerGroupRef.current) {
      layerGroupRef.current = L.layerGroup().addTo(map);
    } else {
      layerGroupRef.current.clearLayers();
      // Re-add to map if it was removed
      if (!map.hasLayer(layerGroupRef.current)) {
        map.addLayer(layerGroupRef.current);
      }
    }

    const treesToRender = treeData.custom;
    let addedCount = 0;

    treesToRender.forEach((tree, index) => {
      const [lat, lon] = convertTreeToLatLon(tree, utmZone);

      if (isNaN(lat) || isNaN(lon)) {
        return;
      }

      const height = getTreeHeight(tree);
      const heightColor = getCustomTreeHeightColor(height);
      const dbh = tree.DIA ? parseFloat(tree.DIA) : (tree.dbh ? parseFloat(tree.dbh) : 'N/A');

      // Use circleMarker for custom trees
      L.circleMarker([lat, lon], {
        radius: 4,
        fillColor: heightColor,
        color: '#1e293b',
        weight: 1,
        opacity: 1,
        fillOpacity: 0.95,
        zIndexOffset: 500
      })
        .bindPopup(`
          <div>
            <strong>Custom Tree ${tree.TREE_ID || index + 1}</strong>
            ${height !== null ? `<div>Height: ${height.toFixed(2)} m</div>` : ''}
            ${dbh !== 'N/A' ? `<div>DBH: ${parseFloat(dbh).toFixed(2)} cm</div>` : ''}
            <div style="font-size: 0.85em; color: #666;">Color: ${heightColor}</div>
          </div>
        `)
        .addTo(layerGroupRef.current);

      addedCount++;
    });

    console.log('CustomTreeMarkersLayer: added', addedCount, 'markers to map');

    return () => {
      // Don't remove on unmount - we want to keep the markers
    };
  }, [map, treeData, showCustomTrees, utmZone]);

  return null;
}

// Separate component for custom tree markers
function CustomTreeMarkers({ treeData, showCustomTrees, utmZone }) {
  console.log('CustomTreeMarkers render called with:', { treeData: !!treeData, custom: treeData?.custom?.length, showCustomTrees });

  if (!treeData?.custom || treeData.custom.length === 0) {
    console.log('CustomTreeMarkers: no data to render');
    return null;
  }

  return <CustomTreeMarkersLayer treeData={treeData} showCustomTrees={showCustomTrees} utmZone={utmZone} />;
}

// Sync maps component - simplified to only set ref
function MapSync({ mapRef }) {
  const map = useMap();

  useEffect(() => {
    if (mapRef) {
      mapRef.current = map;
    }
  }, [map, mapRef]);

  return null;
}



function DualMaps({
  onPolygonCreated,
  onPolygonEdited,
  onPolygonDeleted,
  treeData,
  showFFTrees,
  showCustomTrees,
  utmZone = '26910',
  center = [48.42, -120.15],
  zoom = 12
}) {
  const map1Ref = useRef(null);
  const map2Ref = useRef(null);

  // Debug: log when treeData prop changes and auto-center map
  useEffect(() => {
    console.log('DualMaps: treeData prop changed:', treeData);
  }, [treeData]);

  // Set both maps as ready and sync them
  useEffect(() => {
    console.log('DualMaps: Starting initialization timer...');
    const timer = setTimeout(() => {
      console.log('DualMaps: Checking if maps are ready', {
        map1: !!map1Ref.current,
        map2: !!map2Ref.current
      });

      if (map1Ref.current && map2Ref.current) {
        console.log('DualMaps: Both maps ready, syncing...');

        try {
          // Sync both ways - this creates bidirectional sync
          map1Ref.current.sync(map2Ref.current);
          map2Ref.current.sync(map1Ref.current);
          console.log('✓ Maps synced bidirectionally!');
        } catch (error) {
          console.error('✗ Map sync failed:', error);
        }
      } else {
        console.warn('DualMaps: Not all maps are ready yet');
      }
    }, 1000);

    return () => {
      console.log('DualMaps: Cleanup - unsyncing maps');
      // Cleanup: unsync on unmount
      try {
        if (map1Ref.current && map2Ref.current) {
          if (typeof map1Ref.current.unsync === 'function') {
            map1Ref.current.unsync(map2Ref.current);
          }
          if (typeof map2Ref.current.unsync === 'function') {
            map2Ref.current.unsync(map1Ref.current);
          }
        }
      } catch (error) {
        console.warn('Map unsync cleanup failed:', error);
      }
      clearTimeout(timer);
    };
  }, []);

  const handleCreated = (e) => {
    const { layer } = e;
    const geojson = layer.toGeoJSON();
    if (onPolygonCreated) {
      onPolygonCreated(geojson);
    }
  };

  const handleEdited = (e) => {
    const { layers } = e;
    layers.eachLayer((layer) => {
      const geojson = layer.toGeoJSON();
      if (onPolygonEdited) {
        onPolygonEdited(geojson);
      }
    });
  };

  const handleDeleted = (e) => {
    if (onPolygonDeleted) {
      onPolygonDeleted();
    }
  };

  return (
    <div style={{ display: 'flex', gap: '10px', height: '500px' }}>
      {/* Main Map */}
      <div style={{ flex: 1 }}>
        <MapContainer
          center={center}
          zoom={zoom}
          style={{ height: '100%', width: '100%' }}
        >
          <MapSync mapRef={map1Ref} />
          <TileLayer
            url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
            attribution='&copy; Esri'
          />

          <FeatureGroup>
            <EditControl
              position="topright"
              onCreated={handleCreated}
              onEdited={handleEdited}
              onDeleted={handleDeleted}
              draw={{
                polyline: false,
                circle: false,
                circlemarker: false,
                marker: false,
                rectangle: false,
                polygon: {
                  shapeOptions: {
                    color: '#33ffff',
                    weight: 2,
                    fill: false,
                    fillOpacity: 0
                  }
                }
              }}
            />
          </FeatureGroup>

          {/* Tree markers */}
          <TreeMarkers treeData={treeData} showFFTrees={showFFTrees} utmZone={utmZone} />
          {/* Custom tree markers */}
          <CustomTreeMarkers treeData={treeData} showCustomTrees={showCustomTrees} utmZone={utmZone} />
        </MapContainer>
      </div>

      {/* Secondary Map */}
      <div style={{ flex: 1 }}>
        <MapContainer
          center={center}
          zoom={zoom}
          style={{ height: '100%', width: '100%' }}
        >
          <MapSync mapRef={map2Ref} />
          <TileLayer
            url="https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
            attribution='&copy; Esri'
          />
        </MapContainer>
      </div>
    </div>
  );
}

export default DualMaps;
