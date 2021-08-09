"""
Try and learn how asyncio works
"""

import time
import asyncio
import random

print("Try Python's asyncio")


async def count():
    print("One")
    # "I'm going to sleep for 1 second, let something else meaningful be done in the meanwhile"
    await asyncio.sleep(1)
    print("Two")


async def test_coroutine():
    await asyncio.gather(count(), count(), count())


# ANSI colors

colors = (
    "\033[0m",
    "\033[36m",
    "\033[91m",
    "\033[35m",
)


async def make_random(idx: int, threshold: int = 6) -> int:
    i = random.randint(0, 10)
    while i <= threshold:
        print(colors[idx + 1] + f"makerandom({idx}) == {i} too low; retrying.")
        await asyncio.sleep(idx + 1)
        i = random.randint(0, 10)
    print(colors[idx + 1] + f"---> Finished")
    return i


async def test_random():
    """Gather small coroutines and chain them together"""
    res = await asyncio.gather(*(make_random(i, 10 - i - 1) for i in range(3)))
    return res


if __name__ == "__main__":
    start_time = time.time()
    asyncio.run(test_coroutine())
    duration = time.time() - start_time
    print(f"{__file__} executed in {duration:0.2f} seconds")

    random.seed(444)
    r1, r2, r3 = asyncio.run(test_random())
    print(f"f1: {r1}, f2: {r2}, f3: {r3}")