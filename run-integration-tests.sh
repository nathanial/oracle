#!/bin/bash
# Run OpenRouter integration tests
# Requires .env file with OPENROUTER_API_KEY

set -e

# Load environment variables from .env
if [ -f .env ]; then
    export $(grep -v '^#' .env | xargs)
else
    echo "Error: .env file not found"
    echo "Create a .env file with: OPENROUTER_API_KEY=your-key"
    exit 1
fi

# Enable integration tests
export OPENROUTER_RUN_INTEGRATION=1

# Optional: override model (defaults to claude-3-5-sonnet)
# export OPENROUTER_MODEL=anthropic/claude-3-haiku

echo "Running OpenRouter integration tests..."
lake test
