const express = require('express');
const axios = require('axios');
const router = express.Router();

const FASTFUELS_API_URL = process.env.FASTFUELS_API_URL || 'https://api.fastfuels.silvxlabs.com/v1';

// Helper to create headers
const getHeaders = (apiKey) => ({
  'accept': 'application/json',
  'api-key': apiKey,
  'Content-Type': 'application/json'
});

// Helper to poll for completion
const pollForCompletion = async (url, apiKey, maxAttempts = 60) => {
  for (let i = 0; i < maxAttempts; i++) {
    const response = await axios.get(url, { headers: getHeaders(apiKey) });
    if (response.data.status === 'completed') {
      return response.data;
    }
    if (response.data.status === 'failed') {
      throw new Error('Operation failed');
    }
    await new Promise(resolve => setTimeout(resolve, 5000)); // Wait 5 seconds
  }
  throw new Error('Operation timed out');
};

// Create domain
router.post('/domains', async (req, res) => {
  try {
    const { geojson, apiKey } = req.body;

    if (!apiKey) {
      return res.status(400).json({ error: 'API key is required' });
    }

    console.log(`Creating domain with API URL: ${FASTFUELS_API_URL}`);

    const response = await axios.post(
      `${FASTFUELS_API_URL}/domains`,
      geojson,
      { headers: getHeaders(apiKey) }
    );

    if (response.data.detail) {
      return res.status(400).json({ error: response.data.detail });
    }

    res.json(response.data);
  } catch (error) {
    console.error('Domain creation error:', error.message);
    console.error('Error details:', error.response?.data || error);
    res.status(500).json({ error: error.message, details: error.response?.data });
  }
});

// Create road feature
router.post('/domains/:domainId/features/road', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/features/road`;
    const response = await axios.post(
      url,
      { sources: ['OSM'] },
      { headers: getHeaders(apiKey) }
    );

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Create water feature
router.post('/domains/:domainId/features/water', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/features/water`;
    const response = await axios.post(
      url,
      { sources: ['OSM'] },
      { headers: getHeaders(apiKey) }
    );

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Create tree inventory
router.post('/domains/:domainId/inventories/tree', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey, treeMapVersion, treeMapStyle, sources } = req.body;

    let bodyContent = { sources: sources || ['TreeMap'] };

    if (treeMapStyle === 'fusion') {
      bodyContent.TreeMap = {
        version: treeMapVersion || '2016',
        seed: 123456789,
        canopyHeightMapConfiguration: { source: 'Meta2024' }
      };
    } else {
      bodyContent.TreeMap = {
        version: treeMapVersion || '2016'
      };
    }

    bodyContent.featureMasks = ['road', 'water'];

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/inventories/tree`;
    const response = await axios.post(url, bodyContent, { headers: getHeaders(apiKey) });

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Export tree inventory to CSV
router.post('/domains/:domainId/inventories/tree/exports/csv', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/inventories/tree/exports/csv`;
    const response = await axios.post(url, {}, { headers: getHeaders(apiKey) });

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Upload tree inventory file
router.post('/domains/:domainId/inventories/tree/upload', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/inventories/tree`;
    const response = await axios.post(
      url,
      { sources: ['file'] },
      { headers: getHeaders(apiKey) }
    );

    res.json(response.data);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Create tree grid
router.post('/domains/:domainId/grids/tree', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/grids/tree`;
    const response = await axios.post(
      url,
      {
        attributes: ['bulkDensity', 'fuelMoisture'],
        fuelMoisture: { source: 'uniform', value: 100 }
      },
      { headers: getHeaders(apiKey) }
    );

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Create surface grid
router.post('/domains/:domainId/grids/surface', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/grids/surface`;
    const response = await axios.post(
      url,
      { attributes: ['fuelLoad', 'fuelDepth', 'fuelMoisture'] },
      { headers: getHeaders(apiKey) }
    );

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Create topography grid
router.post('/domains/:domainId/grids/topography', async (req, res) => {
  try {
    const { domainId } = req.params;
    const { apiKey } = req.body;

    const url = `${FASTFUELS_API_URL}/domains/${domainId}/grids/topography`;
    const response = await axios.post(
      url,
      { attributes: ['elevation'] },
      { headers: getHeaders(apiKey) }
    );

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Get QUIC-Fire input files
router.post('/domains/:domainId/grids/exports/:format', async (req, res) => {
  try {
    const { domainId, format } = req.params;
    const { apiKey } = req.body;

    const exportType = format === 'zarr' ? 'zarr' : 'QUIC-Fire';
    const url = `${FASTFUELS_API_URL}/domains/${domainId}/grids/exports/${exportType}`;
    const response = await axios.post(url, {}, { headers: getHeaders(apiKey) });

    const result = await pollForCompletion(url, apiKey);
    res.json(result);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Proxy for downloading CSV files from external URLs (e.g., Google Cloud Storage)

router.get('/proxy-csv', async (req, res) => {
  const { url } = req.query;
  if (!url) return res.status(400).send('Missing url parameter');
  try {
    const response = await axios.get(url, { responseType: 'stream' });
    res.setHeader('Content-Type', 'text/csv');
    response.data.pipe(res);
  } catch (err) {
    res.status(500).send('Failed to fetch file');
  }
});

module.exports = router;
