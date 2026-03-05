import axios from 'axios';

// Get API base URL at runtime (when the app loads in browser)
const getAPIBaseURL = () => {
  // Try environment variable first
  if (process.env.REACT_APP_API_URL) {
    return process.env.REACT_APP_API_URL;
  }

  // Use relative URL - will always use same origin as the frontend
  // This works for both localhost:3001 and production servers
  return '/api';
};

// Create axios instance with the correct base URL
const createAPIClient = () => {
  const baseURL = getAPIBaseURL();
  console.log('API Client using baseURL:', baseURL);

  return axios.create({
    baseURL: baseURL
  });
};

const apiClient = createAPIClient();

export const fastfuelsAPI = {
  createDomain: async (geojson, apiKey) => {
    const response = await apiClient.post(`/fastfuels/domains`, {
      geojson,
      apiKey
    });
    return response.data;
  },

  createRoadFeature: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/features/road`,
      { apiKey }
    );
    return response.data;
  },

  createWaterFeature: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/features/water`,
      { apiKey }
    );
    return response.data;
  },

  createTreeInventory: async (domainId, apiKey, options = {}) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/inventories/tree`,
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
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/inventories/tree/exports/csv`,
      { apiKey }
    );
    return response.data;
  },

  uploadTreeInventory: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/inventories/tree/upload`,
      { apiKey }
    );
    return response.data;
  },

  createTreeGrid: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/grids/tree`,
      { apiKey }
    );
    return response.data;
  },

  createSurfaceGrid: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/grids/surface`,
      { apiKey }
    );
    return response.data;
  },

  createTopographyGrid: async (domainId, apiKey) => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/grids/topography`,
      { apiKey }
    );
    return response.data;
  },

  getInputFiles: async (domainId, apiKey, format = 'zip') => {
    const response = await apiClient.post(
      `/fastfuels/domains/${domainId}/grids/exports/${format}`,
      { apiKey }
    );
    return response.data;
  }
};

export const dataAPI = {
  loadCSV: async (file) => {
    const formData = new FormData();
    formData.append('file', file);
    const response = await apiClient.post(`/data/load-csv`, formData);
    return response.data;
  },

  saveInventory: async (data, domainId) => {
    const response = await apiClient.post(`/data/save-inventory`, {
      data,
      domainId
    });
    return response.data;
  },

  processInventory: async (data, operation, params) => {
    const response = await apiClient.post(`/data/process-inventory`, {
      data,
      operation,
      params
    });
    return response.data;
  },

  getFuelbeds: async () => {
    const response = await apiClient.get(`/data/fuelbeds`);
    return response.data;
  }
};
