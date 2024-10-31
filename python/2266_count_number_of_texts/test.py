import unittest
from main import Solution


class MyTest(unittest.TestCase):

    def test(self):
        tasks = [
            ("", 0),
            ("2", 1),
            ("22", 2),
            ("222", 4),
            ("2222", 7),
            ("22222", 13),
            ("222222", 24),
            ("22233", 8),
            ("222333", 16),
            ("222222222222222222222222222222222222", 82876089),
            ("7777", 8),
            ("77777", 15),
            ("777777", 29),
            ("22277777733", 29*8),
            ("2227777773399999", 29*8*15),
            ("999992227777773399999", 15*29*8*15),
        ]
        for (pressedKeys, expectedResult) in tasks:
            print(pressedKeys, ':', expectedResult)
            result = Solution().countTexts(pressedKeys)
            self.assertEqual(result, expectedResult)

