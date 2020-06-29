package main

import "fmt"

func main() {
	a := [5]int{0}
	fmt.Println("emp:", a)

	a[4] = 100
	fmt.Println("get:", a[4])

	var twoD [2][3]int
	for i := 0; i < 2; i++ {
		for j := 0; j < 3; j++ {
			twoD[i][j] = i + j
		}
	}
	fmt.Println("2d:", twoD)
}
