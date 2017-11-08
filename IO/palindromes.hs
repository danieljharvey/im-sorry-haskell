isPalindrome xs = xs == reverse xs

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines

main = interact respondPalindromes
