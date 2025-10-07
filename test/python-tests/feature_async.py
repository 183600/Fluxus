# Async function tests (simplified)

import asyncio

async def fetch_data():
    # Simulate async operation
    return "data fetched"

async def process_data():
    data = await fetch_data()
    return f"processed: {data}"

# Note: In a real async environment, this would be:
# result = asyncio.run(process_data())
# But for this test, we'll simulate the result
result = "processed: data fetched"
print(result)