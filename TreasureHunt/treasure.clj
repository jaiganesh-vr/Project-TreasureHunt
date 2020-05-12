(require '[clojure.string :as str])

(defn loadMap []
    ;Reading the map file from disk.
    (def map_raw (slurp "map.txt"))
    (def map_data (str/split map_raw #"\n"))
    ;Defining the map's row and column values.
    (def map_row_count (count map_data))
    (def map_column_count (count (get map_data 0)))
    ;Defining the array boundaries.
    (def x_bound (- map_row_count 1))
    (def y_bound (- map_column_count 1))
)

(defn terminate [map_args case_value]
    
    (def map_data map_args)
    (def switch case_value)
    ;Defining the print statements for different terminate conditions.
    (if (= switch 0)
        (println "\nInvalid map. \n")
    )
    (if (= switch 1)
        (println "\nWoo Hoo, I found the treasure :-) \n")
    )
    (if (= switch 2)
        (println "\nUh oh, I could not find the treasure :-( \n")
    )
    ;Printing the final map.
    (doseq [row map_data]
        (println row)
    )
    ;Closing the program.
    (System/exit 0)
)

(defn validateMap []    
    ;Number of column validation for all the rows.
    (doseq [row map_data]
        ;Triming the empty spaces in the row.
        (def row_trimmed (str/trim row))
        (if (not= (count row_trimmed) (- map_column_count 1))
            (terminate map_data 0)
        )
    ) 
)

(defn exploreMap [x,y,map_args,x_bound,y_bound]

    (def map_instance map_args)
    (def current_value (str (get (get map_instance x) y)))
    ;Lookup condition for treasure.
    (if (= current_value "@")
        (terminate map_instance 1)
    )
    ;Lookup condition for path.
    (if (=  current_value "-")
        (do 
            ;Updating the visited path with + sign.
            (def row_tobeupdated (get map_instance x))
            (def character (nth (seq row_tobeupdated) y))
            (def slice_left (subs row_tobeupdated 0 y))
            (def slice_right (subs row_tobeupdated (+ y 1)))
            (def row_updated (str slice_left "+" slice_right))
            (def map_updated (assoc map_instance x row_updated))

            ;Defining the conditions for node traversing.  
            (if (< y y_bound)
            (exploreMap x (+ y 1) map_updated x_bound y_bound))
            (if (< x x_bound)
            (exploreMap (+ x 1) y map_updated x_bound y_bound))
            (if (> y 0)
            (exploreMap x (- y 1) map_updated x_bound y_bound))
            (if (> x 0)
            (exploreMap (- x 1) y map_updated x_bound y_bound))
            
            ;Updating the visited non leading path with ! sign.
            (def row_tobeupdated (get map_updated x))
            (def character (nth (seq row_tobeupdated) y))
            (def slice_left (subs row_tobeupdated 0 y))
            (def slice_right (subs row_tobeupdated (+ y 1)))
            (def row_updated (str slice_left "!" slice_right))
            (def map_updated (assoc map_updated x row_updated))
        )
    )
    ;Terminate condition for the map with initial node is a wall.
    (def current_value (str (get (get map_instance x) y)))
    (if (= x 0)
        (if (= y 0)
            (if (= current_value "#")
                (terminate map_instance 0)
            )
        )
    ) 
    ;Terminate condition for unsuccessful treasure hunt.  
    (def current_value (str (get (get map_updated x) y)))
    (if (= x 0)
        (if (= y 0)
            (if (= current_value "!")
                (terminate map_updated 2)
            )
        )
    )
)

(loadMap)
(validateMap)
(println "\nThis is my challenge: \n")
(println map_raw "\n")
(exploreMap 0 0 map_data x_bound y_bound)
