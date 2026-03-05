# Multi-stage build for FuelsCraft-web
# Stage 1: Build React frontend
FROM node:18-alpine AS frontend-builder

WORKDIR /app

# Copy entire client directory with all dependencies
COPY client ./client

# Build React app
WORKDIR /app/client
RUN npm ci && npm run build

# Verify build succeeded
RUN test -f build/index.html || (echo "ERROR: build/index.html not found!" && exit 1)

# Stage 2: Runtime environment
FROM node:18-alpine

WORKDIR /app

# Install dumb-init for proper signal handling
RUN apk add --no-cache dumb-init

# Copy server files
COPY server ./

# Install production dependencies only
RUN npm ci --production

# Copy built frontend from builder stage
COPY --from=frontend-builder /app/client/build ./client/build

# Verify frontend was copied
RUN test -f client/build/index.html || (echo "ERROR: frontend not copied!" && exit 1)

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
