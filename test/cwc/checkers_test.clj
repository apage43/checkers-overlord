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

(deftest simple-jumps
  (testing "Single jumps are found and forced"
    (let [board (apply-pdns initial-board ["9-14" "23-18"])]
      (is (= #{"14x23"}
             (set (map path->pdn (evaluate-moves board :r))))))
    (let [board (apply-pdns initial-board ["9-14" "23-18" "14x23"])]
      (is (= #{"26x19" "27x18"}
             (set (map path->pdn (evaluate-moves board :b))))))))

(deftest double-jumps
  (testing "Double jumps are found and forced"))
