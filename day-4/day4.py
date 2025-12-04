def parse_map(filename):
    """
    Reads the input file and returns:
        - empty_cells: all '.' positions
        - stones: all '@' positions
        - neighbors: precomputed adjacency sets for each stone
    """
    lines = open(filename, 'r').read().strip().split('\n')

    empty_cells = set()
    stones = set()
    neighbors = {}

    height = len(lines)
    width = len(lines[0])

    # Identify empty cells and stones
    for y in range(height):
        for x in range(width):
            if lines[y][x] == '.':
                empty_cells.add((x, y))
            elif lines[y][x] == '@':
                stones.add((x, y))

    # Precompute 8-direction neighbors for each stone
    for x, y in stones:
        adj = set()

        for dx in (-1, 0, 1):
            for dy in (-1, 0, 1):
                if dx == 0 and dy == 0:
                    continue
                adj.add((x + dx, y + dy))

        neighbors[(x, y)] = adj

    return empty_cells, stones, neighbors


def perform_round(stones, neighbors):
    """
    Returns a set of all stones that should be removed in this round.
    A stone is removed if it has fewer than 4 neighboring stones.
    """
    removed = set()

    for pos in stones:
        if len(neighbors[pos] & stones) < 4:
            removed.add(pos)

    return removed


def solve(filename):
    empty_cells, stones, neighbors = parse_map(filename)

    # --- Part 1 ---
    removed_first_round = perform_round(stones, neighbors)
    print(len(removed_first_round))

    # --- Part 2 ---
    total_removed = 0
    stones = stones.copy()

    while True:
        removed = perform_round(stones, neighbors)
        if not removed:
            break

        stones -= removed
        total_removed += len(removed)

    print(total_removed)


if __name__ == "__main__":
    solve("input.example")
    solve("input")
