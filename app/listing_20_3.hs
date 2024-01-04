import MOExamples
import Mechanics3D
import MultipleObjects

main :: IO ()
main = simulateGloss 1 100 billiardInitial billiardPicture (billiardUpdate eulerCromerMPS 30)
