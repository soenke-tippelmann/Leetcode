
class IterativeSolution:
    """
    Solving Hackerrank Gridland Provinces
    https://www.hackerrank.com/challenges/gridland-provinces/problem?isFullScreen=true
    """
    def countStringsInProvince(self, N: int, s1: str, s2: str) -> int:
        assert len(s1) == N
        assert len(s2) == N

        return self._countStringsInProvince(N, [*s1], [*s2])


    def _countStringsInProvince(self, N: int, s1: list[str], s2: list[str]) -> int:
        assert len(s1) == N
        assert len(s2) == N

        left = self._getStringsInProvince(N, s1, s2)
        right = self._getStringsInProvince(N, s1[::-1], s2[::-1])

        return len(left.union(right))

    def _getStringsInProvince(self, N: int, s1: list[str], s2: list[str]) -> set[str]:
        assert N >= 0
        assert len(s1) == N
        assert len(s2) == N

        top = self._getStringsInProvinceFromCorner(N, s1, s2)
        bottom = self._getStringsInProvinceFromCorner(N, s2, s1)
        return top.union(bottom)

    def _getStringsInProvinceFromCorner(self, N: int, s1: list[str], s2: list[str]) -> set[str]:
        assert N >= 0

        start = "top"
        result = set()
        for ptr in range(N-1, -1, -1):
            if start == 'top':
                roundtrip = "".join(s1[ptr:] + s2[ptr:][::-1])
                down = [ s1[ptr] + s2[ptr] + s for s in result ]
                result = set([roundtrip] + down)
                start = 'bottom'
            elif start == 'bottom':
                roundtrip = "".join(s2[ptr:] + s1[ptr:][::-1])
                up = [ s2[ptr] + s1[ptr] + s for s in result ]
                result = set([roundtrip] + up)
                start = 'bottom'
                start = 'top'

        return result



class RecursiveSolution:
    """
    Solving Hackerrank Gridland Provinces
    https://www.hackerrank.com/challenges/gridland-provinces/problem?isFullScreen=true
    """
    def countStringsInProvince(self, N: int, s1: str, s2: str) -> int:
        assert len(s1) == N
        assert len(s2) == N

        return self._countStringsInProvince(N, [*s1], [*s2])


    def _countStringsInProvince(self, N: int, s1: list[str], s2: list[str]) -> int:
        assert len(s1) == N
        assert len(s2) == N

        left = self._getStringsInProvince(N, s1, s2)
        right = self._getStringsInProvince(N, s1[::-1], s2[::-1])

        return len(left.union(right))

    def _getStringsInProvince(self, N: int, s1: list[str], s2: list[str]) -> set[str]:
        assert N >= 0
        assert len(s1) == N
        assert len(s2) == N

        top = self._getStringsInProvinceFromCorner(N, 0, s1, s2, "top")
        bottom = self._getStringsInProvinceFromCorner(N, 0, s1, s2, "bottom")
        return top.union(bottom)

    def _getStringsInProvinceFromCorner(self, N: int, ptr: int, s1: list[str], s2: list[str], start: str) -> set[str]:
        assert N >= 0
        assert start in ['top', 'bottom']

        if N == 0:
            return set()

        if start == 'top':
            roundtrip = "".join(s1[ptr:] + s2[ptr:][::-1])
            down = [ s1[ptr] + s2[ptr] + s for s in self._getStringsInProvinceFromCorner(N-1,ptr+1,s1,s2,start='bottom') ]
            return set([roundtrip] + down)
        if start == 'bottom':
            roundtrip = "".join(s2[ptr:] + s1[ptr:][::-1])
            up = [ s2[ptr] + s1[ptr] + s for s in self._getStringsInProvinceFromCorner(N-1,ptr+1,s1,s2,start='top') ]
            return set([roundtrip] + up)

        raise Exception("Invalid state")







