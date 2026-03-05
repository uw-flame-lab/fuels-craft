# Multi-stage build for FuelsCraft-web
# Stage 1: Build React frontend
FROM node:18-alpine AS frontend-builder

WORKDIR /app

# Copy entire client directory
COPY client ./client

WORKDIR /app/client

# Install dependencies
RUN npm ci

# Build React app
RUN npm run build && ls -la build/

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

# Verify build was copied
RUN ls -la client/build/ || echo "WARNING: build directory not found"

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

# Start the server
CMD ["node", "index.js"]
