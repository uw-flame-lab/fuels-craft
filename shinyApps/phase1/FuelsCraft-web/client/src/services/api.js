import axios from 'axios';

// Dynamically determine API base URL
// In production, use the current window location
// In development, use localhost:3001
const getAPIBaseURL = () => {
  if (process.env.REACT_APP_API_URL) {
    return process.env.REACT_APP_API_URL;
  }
  
  // Production: use current server
  if (window.location.hostname !== 'localhost' && window.location.hostname !== '127.0.0.1') {
    return `/api`; // Relative URL - will use same host/port as frontend
  }
  
  // Development: use localhost:3001
  return 'http://localhost:3001/api';
};

const API_BASE_URL = getAPIBaseURL();

export const fastfuelsAPI = {
  createDomain: async (geojson, apiKey) => {
    const response = await axios.post(`${API_BASE_URL}/fastfuels/domains`, {
      geojson,
      apiKey
    });
    return response.data;
  },

  createRoadFeature: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/features/road`,
      { apiKey }
    );
    return response.data;
  },

  createWaterFeature: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/features/water`,
      { apiKey }
    );
    return response.data;
  },

  createTreeInventory: async (domainId, apiKey, options = {}) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/inventories/tree`,
      {
        apiKey,
        treeMapVersion: options.treeMapVersion || '2016',
        treeMapStyle: options.treeMapStyle || 'fusion',
        sources: options.sources || ['TreeMap']
      }
    );
    return response.data;
  },

  exportTreeInventory: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/inventories/tree/exports/csv`,
      { apiKey }
    );
    return response.data;
  },

  uploadTreeInventory: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/inventories/tree/upload`,
      { apiKey }
    );
    return response.data;
  },

  createTreeGrid: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/grids/tree`,
      { apiKey }
    );
    return response.data;
  },

  createSurfaceGrid: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/grids/surface`,
      { apiKey }
    );
    return response.data;
  },

  createTopographyGrid: async (domainId, apiKey) => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/grids/topography`,
      { apiKey }
    );
    return response.data;
  },

  getInputFiles: async (domainId, apiKey, format = 'zip') => {
    const response = await axios.post(
      `${API_BASE_URL}/fastfuels/domains/${domainId}/grids/exports/${format}`,
      { apiKey }
    );
    return response.data;
  }
};

export const dataAPI = {
  loadCSV: async (file) => {
    const formData = new FormData();
    formData.append('file', file);
    const response = await axios.post(`${API_BASE_URL}/data/load-csv`, formData);
    return response.data;
  },

  saveInventory: async (data, domainId) => {
    const response = await axios.post(`${API_BASE_URL}/data/save-inventory`, {
      data,
      domainId
    });
    return response.data;
  },

  processInventory: async (data, operation, params) => {
    const response = await axios.post(`${API_BASE_URL}/data/process-inventory`, {
      data,
      operation,
      params
    });
    return response.data;
  },

  getFuelbeds: async () => {
    const response = await axios.get(`${API_BASE_URL}/data/fuelbeds`);
    return response.data;
  }
};
