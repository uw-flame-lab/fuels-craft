const express = require('express');
const cors = require('cors');
const session = require('express-session');
require('dotenv').config();

const fastfuelsRoutes = require('./routes/fastfuels');
const dataRoutes = require('./routes/data');

const app = express();
const PORT = process.env.PORT || 3001;

// Middleware
app.use(cors({
  origin: 'http://localhost:3000',
  credentials: true
}));
app.use(express.json({ limit: '50mb' }));
app.use(express.urlencoded({ extended: true, limit: '50mb' }));

// Session management
app.use(session({
  secret: 'fuelscraft-secret-key-change-in-production',
  resave: false,
  saveUninitialized: true,
  cookie: { secure: false } // Set to true if using HTTPS
}));

// Routes
app.use('/api/fastfuels', fastfuelsRoutes);
app.use('/api/data', dataRoutes);

// Health check
app.get('/api/health', (req, res) => {
  res.json({ status: 'ok', timestamp: new Date().toISOString() });
});

app.listen(PORT, () => {
  console.log(`FuelsCraft server running on port ${PORT}`);
});

