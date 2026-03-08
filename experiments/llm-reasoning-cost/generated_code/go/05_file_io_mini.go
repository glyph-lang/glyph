package main

import (
	"fmt"
	"os"
)

type Result struct {
	Message string
	Error   error
}

func writeGreeting(filename string) Result {
	file, err := os.Create(filename)
	if err != nil {
		return Result{"", err}
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil {
			err = closeErr
		}
	}()

	_, err = file.WriteString("Hello from the program!")
	if err != nil {
		return Result{"", err}
	}

	return Result{"File written successfully", nil}
}

func main() {
	result := writeGreeting("output.txt")
	if result.Error != nil {
		fmt.Println("Error:", result.Error)
	} else {
		fmt.Println(result.Message)
	}
}
