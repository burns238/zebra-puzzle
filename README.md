# Zebra Puzzle Solution

My solution to the zebra puzzle on exercism. My original solution was to have each rule as a function in a sequence, get all possible permutations and then filter the permutations using a fold left over the functions. Trying to create every single permutation resulted in an out of memory exception and after looking at another solution, this brute force method is still feasible you just need to apply the rules after each line of the for comprehension to cut down on the permutations we check.

## Zebra Puzzle

Solve the zebra puzzle.

1. There are five houses.
2. The Englishman lives in the red house.
3. The Spaniard owns the dog.
4. Coffee is drunk in the green house.
5. The Ukrainian drinks tea.
6. The green house is immediately to the right of the ivory house.
7. The Old Gold smoker owns snails.
8. Kools are smoked in the yellow house.
9. Milk is drunk in the middle house.
10. The Norwegian lives in the first house.
11. The man who smokes Chesterfields lives in the house next to the man with the fox.
12. Kools are smoked in the house next to the house where the horse is kept.
13. The Lucky Strike smoker drinks orange juice.
14. The Japanese smokes Parliaments.
15. The Norwegian lives next to the blue house.

Each of the five houses is painted a different color, and their
inhabitants are of different national extractions, own different pets,
drink different beverages and smoke different brands of cigarettes.

Which of the residents drinks water?
Who owns the zebra?

The Scala exercises assume an SBT project scheme. The exercise solution source
should be placed within the exercise directory/src/main/scala. The exercise
unit tests can be found within the exercise directory/src/test/scala.

To run the tests simply run the command `sbt test` in the exercise directory.

For more detailed info about the Scala track see the [help
page](http://exercism.io/languages/scala).


## Source

Wikipedia [https://en.wikipedia.org/wiki/Zebra_Puzzle](https://en.wikipedia.org/wiki/Zebra_Puzzle)

