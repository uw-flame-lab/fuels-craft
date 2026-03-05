#!/bin/bash

echo "🚀 Starting FuelsCraft Web Application"
echo "======================================"

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if we're in the right directory
if [ ! -f "README.md" ]; then
    echo "❌ Please run this script from the FuelsCraft-web directory"
    exit 1
fi

# Function to kill background processes on exit
cleanup() {
    echo -e "\n${BLUE}Shutting down servers...${NC}"
    kill $SERVER_PID 2>/dev/null
    kill $CLIENT_PID 2>/dev/null
    exit 0
}

trap cleanup EXIT INT TERM

# Check if dependencies are installed
if [ ! -d "server/node_modules" ]; then
    echo "📦 Installing server dependencies..."
    cd server && npm install && cd ..
fi

if [ ! -d "client/node_modules" ]; then
    echo "📦 Installing client dependencies..."
    cd client && npm install && cd ..
fi

# Check for .env file
if [ ! -f "server/.env" ]; then
    echo "⚙️  Creating .env file..."
    cp server/.env.example server/.env
    echo "⚠️  Please update server/.env with your FastFuels API key"
    echo "   Press Enter to continue or Ctrl+C to exit and configure..."
    read
fi

# Start backend server
echo -e "${BLUE}Starting backend server on port 3001...${NC}"
cd server
npm start > ../logs/server.log 2>&1 &
SERVER_PID=$!
cd ..

# Wait for server to start
sleep 3

# Check if server started successfully
if ! lsof -ti:3001 > /dev/null; then
    echo "❌ Backend server failed to start. Check logs/server.log"
    exit 1
fi

echo -e "${GREEN}✅ Backend server started (PID: $SERVER_PID)${NC}"

# Start frontend
echo -e "${BLUE}Starting frontend on port 3000...${NC}"
cd client
npm start > ../logs/client.log 2>&1 &
CLIENT_PID=$!
cd ..

# Wait for client to start
sleep 5

echo -e "${GREEN}✅ Frontend started (PID: $CLIENT_PID)${NC}"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo -e "${GREEN}🌲 FuelsCraft is running!${NC}"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "  🌐 Frontend: http://localhost:3000"
echo "  🔧 Backend:  http://localhost:3001"
echo ""
echo "  📝 Logs:"
echo "     - Server: logs/server.log"
echo "     - Client: logs/client.log"
echo ""
echo "  Press Ctrl+C to stop all servers"
echo ""

# Keep script running
wait
