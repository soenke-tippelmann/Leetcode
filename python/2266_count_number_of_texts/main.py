
class Solution:
    """
    Solving Leet Code 2266. Count Number of Texts
    https://leetcode.com/problems/count-number-of-texts/
    """
    def countTexts(self, pressedKeys: str) -> int:
        if not pressedKeys:
            return 0

        counts, maxCount = self.__getCounts(pressedKeys)
        #print("counts", counts, maxCount)

        result = \
            self.__calculateCombinations(counts[3], maxCount[3], [1,2,4]) * \
            self.__calculateCombinations(counts[4], maxCount[4], [1,2,4,8])
        return result


    def __calculateCombinations(self, counts: dict[int, int], maxCount: int, previousValues: list[int]):
        result = 1

        def assignCount(count: int, value: int):
            nonlocal result
            try:
                result *= value ** counts[count]
            except:
                pass

        for c, v in enumerate(previousValues, 1): 
            assignCount(c, v)

        for c in range(len(previousValues) + 1, maxCount + 1):
            newValue = sum(previousValues) % (10**9 + 7)
            assignCount(c, newValue)
            # could use ringbuffer instead!
            previousValues.pop(0)
            previousValues.append(newValue)

        return result

    def __getCounts(self, pressedKeys: str) -> (dict[int, dict[int, int]], dict[int, int]):
        counts = {3: dict(), 4: dict()}
        maxCount = {3: 0, 4: 0}

        def inc(count: int, key: str):
            combs = 3
            if key in ['7', '9']:
                combs = 4

            nonlocal maxCount
            if count > maxCount[combs]:
                maxCount[combs] = count

            try:
                counts[combs][count] += 1
            except:
                counts[combs][count] = 1

        previousKey = 0
        count = 0
        for key in pressedKeys:
            if key == previousKey:
                count += 1
            else:
                if previousKey != 0:
                    inc(count, previousKey)
                previousKey = key
                count = 1
        inc(count, previousKey)

        return counts, maxCount

