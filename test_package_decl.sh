#!/bin/bash

echo "Running Go Package Declaration Tests..."
stack test --test-arguments "--match 'Package Declaration'" 2>&1
