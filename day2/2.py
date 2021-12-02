with open ("input.txt") as fp:
    lines = fp.readlines ()
    aim = 0
    length = 0
    depth = 0
    for line in lines:
        #print(line)
        direction, amount = line.split(' ')
        if direction == "forward":
            length += int (amount)
            depth += int (amount) * aim
        elif direction == "up":
            aim -= int (amount)
        elif direction == "down":
            aim += int (amount)
    print (f"Length: {length} Depth: {depth}")
    print (f"Combined: {length * depth}")
