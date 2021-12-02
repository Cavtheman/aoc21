with open ("input.txt") as fp:
    lines = fp.readlines ()
    length = 0
    depth = 0
    for line in lines:
        #print(line)
        direction, amount = line.split(' ')
        if direction == "forward":
            length += int (amount)
        elif direction == "up":
            depth -= int (amount)
        elif direction == "down":
            depth += int (amount)
    print (f"Length: {length} Depth: {depth}")
    print (f"Combined: {length * depth}")
