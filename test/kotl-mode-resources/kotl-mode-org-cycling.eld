;; Resource used in
;; "../kotl-mode-tests.el#kotl-mode--org-cycling"
;; Initial buffer contents: "kotl-file.kotl"
;; Below list of (Test name, key, expected visible buffer contents)

(
 ("FOLDED"
  "<tab>"
  "\
   1. A...

   2. D
      d

     2a. E
         e

       2a1. F
            f

")

 ("CHILDREN"
  "<tab>"
  "\
   1. A...

     1a. B...

   2. D
      d

     2a. E
         e

       2a1. F
            f

")

 ("SUBTREE"
  "<tab>"
  "\
   1. A
      a

     1a. B
         b

       1a1. C
            c

   2. D
      d

     2a. E
         e

       2a1. F
            f

")

 ("Back to FOLDED"
  "<tab>"
  "\
   1. A...

   2. D
      d

     2a. E
         e

       2a1. F
            f

")

 ("OVERVIEW"
  "S-<tab>"
  "\
   1. A...

   2. D...

")

  ("CONTENTS"
  "S-<tab>"
  "\
   1. A...

     1a. B...

       1a1. C...

   2. D...

     2a. E...

       2a1. F...

")

 ("SHOW ALL"
  "S-<tab>"
  "\
   1. A
      a

     1a. B
         b

       1a1. C
            c

   2. D
      d

     2a. E
         e

       2a1. F
            f

")

 ("Back to OVERVIEW"
  "S-<tab>"
  "\
   1. A...

   2. D...

")

)
