import React, { useState } from 'react';

const UTM_ZONES = {
  26910: '10N Western coastal U.S.',
  26911: '11N CA, NV, UT, AZ',
  26912: '12N CO, NM, WY',
  26913: '13N Central Rockies',
  26914: '14N Great Plains',
  26915: '15N Midwest',
  26916: '16N IL, KY, TN',
  26917: '17N GA, FL, Carolinas',
  26918: '18N Mid-Atlantic',
  26919: '19N New England'
};

function Sidebar({
  onCreateDomain,
  onCreateRoadFeature,
  onCreateWaterFeature,
  onCreateTreeInventory,
  onExportTreeInventory,
  onCreateTreeGrid,
  onCreateSurfaceGrid,
  onCreateTopographyGrid,
  onGetInputFiles,
  onLoadCustomFile,
  onClearMap,
  domainId,
  setDomainId,
  apiKey,
  setApiKey,
  treeMapVersion,
  setTreeMapVersion,
  treeMapStyle,
  setTreeMapStyle,
  qfInputFormat,
  setQfInputFormat,
  utmZone,
  setUtmZone,
  customTreeFile,
  setCustomTreeFile,
  loading,
  messages
}) {
  const [fileInput, setFileInput] = useState(null);

  return (
    <div style={{
      width: '350px',
      padding: '20px',
      backgroundColor: '#f5f5f5',
      overflowY: 'auto',
      height: '100vh'
    }}>
      <h2>FuelsCraft</h2>

      {messages.area && (
        <pre style={{
          padding: '10px',
          backgroundColor: '#fff',
          border: '1px solid #ddd',
          fontSize: '12px',
          maxHeight: '100px',
          overflow: 'auto'
        }}>
          {messages.area}
        </pre>
      )}

      <button
        onClick={onClearMap}
        style={{ marginBottom: '10px', width: '100%' }}
      >
        Clear Map
      </button>

      <div style={{ marginBottom: '15px' }}>
        <label>Select UTM Zone</label>
        <select
          value={utmZone}
          onChange={(e) => setUtmZone(e.target.value)}
          style={{ width: '100%', padding: '5px' }}
        >
          {Object.entries(UTM_ZONES).map(([code, name]) => (
            <option key={code} value={code}>{name}</option>
          ))}
        </select>
      </div>

      <hr />

      <div style={{ marginBottom: '15px' }}>
        <label>CHM or Inventory File</label>
        <input
          type="text"
          value={customTreeFile}
          onChange={(e) => setCustomTreeFile(e.target.value)}
          style={{ width: '100%', padding: '5px', marginBottom: '5px' }}
        />
        <input
          type="file"
          onChange={(e) => setFileInput(e.target.files[0])}
          style={{ marginBottom: '5px' }}
        />
        <button onClick={() => onLoadCustomFile(fileInput || null)} style={{ width: '100%' }}>
          Load
        </button>
      </div>

      <h4>FastFuels API</h4>

      <div style={{ marginBottom: '15px' }}>
        <label>API Key</label>
        <input
          type="text"
          value={apiKey}
          onChange={(e) => setApiKey(e.target.value)}
          style={{ width: '100%', padding: '5px' }}
        />
      </div>

      <button
        onClick={onCreateDomain}
        disabled={loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        {loading ? 'Creating...' : 'Create Domain'}
      </button>

      <div style={{ marginBottom: '15px' }}>
        <label>Domain ID</label>
        <input
          type="text"
          value={domainId}
          onChange={(e) => setDomainId(e.target.value)}
          style={{ width: '100%', padding: '5px' }}
          readOnly
        />
      </div>

      <button
        onClick={onCreateRoadFeature}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Road Feature
      </button>
      {messages.roadFeature && <div style={{ fontSize: '12px', color: 'green' }}>{messages.roadFeature}</div>}

      <button
        onClick={onCreateWaterFeature}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Water Feature
      </button>
      {messages.waterFeature && <div style={{ fontSize: '12px', color: 'green' }}>{messages.waterFeature}</div>}

      <div style={{ marginBottom: '15px' }}>
        <label>TreeMap Version</label>
        <div>
          <input
            type="radio"
            value="2014"
            checked={treeMapVersion === '2014'}
            onChange={(e) => setTreeMapVersion(e.target.value)}
          /> 2014
          <input
            type="radio"
            value="2016"
            checked={treeMapVersion === '2016'}
            onChange={(e) => setTreeMapVersion(e.target.value)}
            style={{ marginLeft: '10px' }}
          /> 2016
        </div>
      </div>

      <div style={{ marginBottom: '15px' }}>
        <label>Method</label>
        <div>
          <input
            type="radio"
            value="raw"
            checked={treeMapStyle === 'raw'}
            onChange={(e) => setTreeMapStyle(e.target.value)}
          /> raw
          <input
            type="radio"
            value="fusion"
            checked={treeMapStyle === 'fusion'}
            onChange={(e) => setTreeMapStyle(e.target.value)}
            style={{ marginLeft: '10px' }}
          /> fusion
        </div>
      </div>

      <button
        onClick={onCreateTreeInventory}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Tree Inventory
      </button>
      {messages.treeInventory && <div style={{ fontSize: '12px', color: 'green' }}>{messages.treeInventory}</div>}

      <button
        onClick={onExportTreeInventory}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Export Tree Inventory
      </button>
      {messages.exportUrl && (
        <div style={{ fontSize: '11px', wordBreak: 'break-all', color: 'blue' }}>
          <a href={messages.exportUrl} target="_blank" rel="noopener noreferrer">
            Download CSV
          </a>
        </div>
      )}

      <hr />

      <button
        onClick={onCreateTreeGrid}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Tree Grid
      </button>
      {messages.treeGrid && <div style={{ fontSize: '12px', color: 'green' }}>{messages.treeGrid}</div>}

      <button
        onClick={onCreateSurfaceGrid}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Surface Grid
      </button>
      {messages.surfaceGrid && <div style={{ fontSize: '12px', color: 'green' }}>{messages.surfaceGrid}</div>}

      <button
        onClick={onCreateTopographyGrid}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Create Topography Grid
      </button>
      {messages.topographyGrid && <div style={{ fontSize: '12px', color: 'green' }}>{messages.topographyGrid}</div>}

      <button
        onClick={onGetInputFiles}
        disabled={!domainId || loading}
        style={{ width: '100%', marginBottom: '10px' }}
      >
        Get QUICFire Input Files
      </button>
      {messages.inputFiles && (
        <div style={{ fontSize: '11px', wordBreak: 'break-all', color: 'blue' }}>
          {messages.inputFiles.startsWith('http') ? (
            <a href={messages.inputFiles} target="_blank" rel="noopener noreferrer">
              Download Files
            </a>
          ) : (
            messages.inputFiles
          )}
        </div>
      )}

      <hr />

      <div style={{ marginBottom: '15px' }}>
        <label>QF Input Format</label>
        <div>
          <input
            type="radio"
            value="zip"
            checked={qfInputFormat === 'zip'}
            onChange={(e) => setQfInputFormat(e.target.value)}
          /> zip
          <input
            type="radio"
            value="zarr"
            checked={qfInputFormat === 'zarr'}
            onChange={(e) => setQfInputFormat(e.target.value)}
            style={{ marginLeft: '10px' }}
          /> zarr
        </div>
      </div>

      {loading && (
        <div style={{
          position: 'fixed',
          top: 0,
          left: 0,
          right: 0,
          bottom: 0,
          backgroundColor: 'rgba(0,0,0,0.5)',
          display: 'flex',
          alignItems: 'center',
          justifyContent: 'center',
          zIndex: 9999
        }}>
          <div style={{
            backgroundColor: 'white',
            padding: '20px',
            borderRadius: '5px'
          }}>
            Loading...
          </div>
        </div>
      )}
    </div>
  );
}

export default Sidebar;
