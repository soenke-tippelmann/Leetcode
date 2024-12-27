import unittest
from main import RecursiveSolution, IterativeSolution


class MyTest(unittest.TestCase):

    def test(self):
        tasks = [
            (1, "a", "a", 1),
            (3, "dab", "abd", 8),
            (5, "ababa", "babab", 2)
        ]

        for (N, s1, s2, expectedResult) in tasks:
            print(N, s1, s2, ':', expectedResult)
            result = RecursiveSolution().countStringsInProvince(N, s1, s2)
            self.assertEqual(result, expectedResult)

        for (N, s1, s2, expectedResult) in tasks:
            print(N, s1, s2, ':', expectedResult)
            result = IterativeSolution().countStringsInProvince(N, s1, s2)
            self.assertEqual(result, expectedResult)
