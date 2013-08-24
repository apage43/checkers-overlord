(ns cwc.checkers-test
  (:require [clojure.test :refer :all]
            [cwc.checkers :refer :all]))

(deftest notation
  (testing "Correct compaction of path"
    (is (= (path->compact-idx
             [[ [2 1] [4 3] ]
              [ [4 3] [6 1] ]])
           [9 18 25])))
  (testing "Correct notation of jumps"
    (is (= (path->pdn [[ [2 1] [4 3] ]
                       [ [4 3] [6 1] ]])
           "9x18x25"))))

(deftest opening-moves
  (testing "Move search finds all opening moves"
    (is (= #{"9-13" "9-14" "10-14" "10-15" "11-15" "11-16" "12-16"}
           (set (map path->pdn (evaluate-moves initial-board :r)))))))
