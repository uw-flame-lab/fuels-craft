const express = require('express');
const multer = require('multer');
const Papa = require('papaparse');
const fs = require('fs').promises;
const path = require('path');
const router = express.Router();

// Configure multer for file uploads
const upload = multer({
  dest: 'uploads/',
  limits: { fileSize: 50 * 1024 * 1024 } // 50MB limit
});

// Load CSV file and parse
router.post('/load-csv', upload.single('file'), async (req, res) => {
  try {
    const filePath = req.file ? req.file.path : req.body.filePath;

    if (!filePath) {
      return res.status(400).json({ error: 'No file provided' });
    }

    const fileContent = await fs.readFile(filePath, 'utf-8');

    Papa.parse(fileContent, {
      header: true,
      dynamicTyping: true,
      complete: (results) => {
        // Clean up uploaded file
        if (req.file) {
          fs.unlink(filePath).catch(console.error);
        }
        res.json({
          data: results.data,
          meta: results.meta
        });
      },
      error: (error) => {
        res.status(500).json({ error: error.message });
      }
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Save tree inventory to CSV
router.post('/save-inventory', async (req, res) => {
  try {
    const { data, domainId } = req.body;

    if (!data || !Array.isArray(data)) {
      return res.status(400).json({ error: 'Invalid data format' });
    }

    const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
    const filename = `tree_inventory_${domainId || 'custom'}_${timestamp}.csv`;
    const filepath = path.join('uploads', filename);

    // Ensure uploads directory exists
    await fs.mkdir('uploads', { recursive: true });

    const csv = Papa.unparse(data);
    await fs.writeFile(filepath, csv);

    res.json({
      filename,
      path: filepath,
      downloadUrl: `/api/data/download/${filename}`
    });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Download file
router.get('/download/:filename', async (req, res) => {
  try {
    const { filename } = req.params;
    const filepath = path.join('uploads', filename);

    res.download(filepath);
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Process tree inventory (coordinate transformations, filtering, etc.)
router.post('/process-inventory', async (req, res) => {
  try {
    const { data, operation, params } = req.body;

    let processedData = [...data];

    switch (operation) {
      case 'filter-height':
        processedData = data.filter(row =>
          row.HT >= params.min && row.HT <= params.max
        );
        break;

      case 'remove-by-polygon':
        // Filter out trees within polygon bounds
        // This would need proper point-in-polygon check
        processedData = data.filter(row => {
          // Implement point-in-polygon logic here
          return true; // Placeholder
        });
        break;

      case 'merge':
        processedData = [...data, ...params.additionalData];
        break;

      default:
        return res.status(400).json({ error: 'Unknown operation' });
    }

    res.json({ data: processedData });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

// Load fuelbed XML data
router.get('/fuelbeds', async (req, res) => {
  try {
    const fuelbedDir = path.join(__dirname, '../../fuelbeds');
    const files = await fs.readdir(fuelbedDir);
    const xmlFiles = files.filter(f => f.endsWith('.xml'));

    res.json({ fuelbeds: xmlFiles });
  } catch (error) {
    res.status(500).json({ error: error.message });
  }
});

module.exports = router;
