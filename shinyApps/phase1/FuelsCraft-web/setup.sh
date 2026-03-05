#!/bin/bash

echo "🌲 FuelsCraft Web Application Setup 🌲"
echo "======================================"

# Check if Node.js is installed
if ! command -v node &> /dev/null; then
    echo "❌ Node.js is not installed. Please install Node.js 16+ first."
    exit 1
fi

echo "✅ Node.js version: $(node --version)"
echo ""

# Setup backend
echo "📦 Installing backend dependencies..."
cd server
npm install
if [ ! -f .env ]; then
    cp .env.example .env
    echo "⚙️  Created .env file - please update with your FastFuels API key"
fi
cd ..

# Setup frontend
echo "📦 Installing frontend dependencies..."
cd client
npm install
cd ..

echo ""
echo "✅ Setup complete!"
echo ""
echo "To start the application:"
echo "  1. Backend:  cd server && npm start"
echo "  2. Frontend: cd client && npm start"
echo ""
echo "Or use Docker: docker-compose up"
echo ""
echo "📝 Don't forget to update server/.env with your FastFuels API key!"
