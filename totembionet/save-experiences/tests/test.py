import save_experiences
import unittest


class Test(unittest.TestCase):
    def test_add_bd(self):
        save_experiences.save().saveExperience("tests","myId")


if __name__ == '__main__':
    unittest.main()