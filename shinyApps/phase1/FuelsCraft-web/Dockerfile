# Multi-stage build for FuelsCraft-web
# Stage 1: Build React frontend
FROM node:18-alpine AS frontend-builder

ARG REACT_APP_API_URL=http://localhost:3001/api

WORKDIR /app

# Copy entire client directory
COPY client ./client

WORKDIR /app/client

# Install dependencies
RUN npm ci

# Build React app with environment variable
RUN REACT_APP_API_URL=${REACT_APP_API_URL} npm run build

# List contents to debug
RUN echo "Build directory contents:" && ls -la build/ 2>&1 || echo "ERROR: build directory missing!"

# Stage 2: Runtime environment
FROM node:18-alpine

WORKDIR /app

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Copy server files
COPY server/package*.json ./

# Install production dependencies only
RUN npm ci --production

# Copy server source
COPY server ./

# Copy built frontend from builder stage
COPY --from=frontend-builder /app/client/build ./client/build

# Create uploads directory with proper permissions
RUN mkdir -p /app/uploads && chmod 777 /app/uploads

# Create non-root user for security
RUN addgroup -g 1001 -S nodejs
RUN adduser -S nodejs -u 1001

# Change ownership of app directory to nodejs user
RUN chown -R nodejs:nodejs /app

USER nodejs

# Expose port
EXPOSE 3001

# Use dumb-init to handle signals properly
ENTRYPOINT ["dumb-init", "--"]
CMD ["node", "index.js"]

# Use dumb-init to handle signals properly
ENTRYPOINT ["dumb-init", "--"]

# Start the server
CMD ["node", "index.js"]
