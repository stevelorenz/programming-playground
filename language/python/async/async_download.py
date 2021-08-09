"""
Example of using Python's standard asyncio and aiohttp
"""

import asyncio
import time
import aiohttp


async def download_site(session, url):
    async with session.get(url) as response:
        print("Read {0} from {1}".format(response.content_length, url))


async def download_all_sites(sites):
    # The session is shared across all tasks, so a context manager is used.
    async with aiohttp.ClientSession() as session:
        tasks = []
        for url in sites:
            task = asyncio.ensure_future(download_site(session, url))
            tasks.append(task)
        # Keep the session alive until all tasks finished.
        await asyncio.gather(*tasks, return_exceptions=True)


if __name__ == "__main__":
    sites = ["https:www.jython.org", "http://olympus.realpython.org/dice"] * 10
    start_time = time.time()

    asyncio.get_event_loop().run_until_complete(download_all_sites(sites))

    duration = time.time() - start_time
    print(f"Downloaded {len(sites)} in {duration} seconds")
