#!/bin/bash

# Startup script for the Prolog synthesis system
# This script starts both the Prolog API server and the frontend HTTP server

echo "🚀 Starting Synthesis Explorer System..."

# Check if SWI-Prolog is installed
if ! command -v swipl &> /dev/null; then
    echo "❌ SWI-Prolog is not installed. Please install it first."
    exit 1
fi

# Check if Python is available
if ! command -v python3 &> /dev/null; then
    echo "❌ Python 3 is not installed. Please install it first."
    exit 1
fi

# --- Pre-flight Check: Kill existing processes on the ports ---
PROLOG_PORT=8083
PYTHON_PORT=3000

echo "🔎 Checking for existing processes on ports $PROLOG_PORT and $PYTHON_PORT..."
# The `|| true` prevents the script from exiting if no process is found
(lsof -ti :$PROLOG_PORT | xargs kill -9) >/dev/null 2>&1 || true
(lsof -ti :$PYTHON_PORT | xargs kill -9) >/dev/null 2>&1 || true
sleep 1 # Give a moment for ports to be released

# Function to kill processes on exit
cleanup() {
    echo "🛑 Shutting down servers..."
    kill $PROLOG_PID 2>/dev/null
    kill $PYTHON_PID 2>/dev/null
    exit 0
}

# Set up trap to catch Ctrl+C
trap cleanup SIGINT SIGTERM

# Start Prolog API server
echo "📡 Starting Prolog API server on port 8083..."
swipl -g "main" working_server.pl &
PROLOG_PID=$!

# Wait a moment for Prolog server to start
sleep 2

# Test if Prolog server is running
if curl -s http://localhost:8083/test > /dev/null; then
    echo "✅ Prolog API server is running at http://localhost:8083"
else
    echo "⚠️  Prolog server may not be fully ready yet..."
fi

# Start Python HTTP server
echo "🌐 Starting frontend HTTP server on port 3000..."
python3 serve_local.py &
PYTHON_PID=$!

# Wait a moment for Python server to start
sleep 1

echo ""
echo "🎉 System is ready!"
echo "📱 Open your browser and go to: http://localhost:3000"
echo "🔧 API server is at: http://localhost:8083"
echo "📋 Press Ctrl+C to stop both servers"
echo ""

# Wait for processes to finish or be interrupted
wait