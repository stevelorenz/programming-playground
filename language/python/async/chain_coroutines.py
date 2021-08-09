import random
import asyncio
import time


async def part1(n: int) -> str:
    i = random.randint(0, 5)
    print(f"part1({n}) sleeping for {i} seconds")
    await asyncio.sleep(i)
    result = f"result{n}-1"
    print(f"Returning part1({n}) == {result}")
    return result


async def part2(n: int, arg: str) -> str:
    i = random.randint(0, 5)
    print(f"part2({n, arg}) sleeping for {i} seconds")
    await asyncio.sleep(i)
    result = f"result{n}-2 derived from {arg}"
    print(f"Returning part2({n, arg}) == {result}")
    return result


async def chain(n: int) -> None:
    start = time.perf_counter()
    p1 = await part1(n)
    p2 = await part2(n, p1)
    end = time.perf_counter() - start
    print(f"--> Chained result{n} => {p2} (took {end:0.2f} seconds)")


async def main(*args):
    """The runtime of main should be the maximum runtime of
    all tasks that are gathered here.
    """
    await asyncio.gather(*(chain(n) for n in args))


if __name__ == "__main__":
    random.seed(time.time())
    args = [1, 2, 3, 4, 5]
    start = time.perf_counter()
    asyncio.run(main(args))
    end = time.perf_counter() - start
    print(f"Program finished in {end:0.2f} seconds")
