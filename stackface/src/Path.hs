module Path
    ( someFunc
    ) where

-- Node with some sort of key
data Node a = Node

-- first node key, second node key, distance
data Edge a = Edge a a Int

-- graph contains a list of edges and a list of nodes
data Graph a = Graph [Node a] [Edge a]

someFunc :: IO ()
someFunc = putStrLn $ "horse"

nodes = [ Node "a", Node "b", Node "c", Node "d", Node "e", Node "f", Node "g", Node "h", Node "i" ]

edges = [ Edge "a" "b" 10, Edge "b" "c" 5, Edge "c" "d" 11, Edge "d" "e" 12, Edge "e" "f" 10, Edge "f" "g" 10, Edge "g" "h" 10, Edge "h" "i" 10, Edge "d" "h" 50 ]

graph :: Graph String
graph = Graph nodes edges