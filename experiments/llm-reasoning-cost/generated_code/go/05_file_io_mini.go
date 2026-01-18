```go
package main

import (
	"fmt"
	"os"
)

func writeGreeting(filename string) error {
	file, err := os.Create(filename)
	if err != nil {
		return fmt.Errorf("failed to create file: %w", err)
	}
	defer func() {
		if closeErr := file.Close(); closeErr != nil {
			fmt.Printf("error closing file: %v\n", closeErr)
		}
	}()

	_, err = file.WriteString("Hello from the program!")
	if err != nil {
		return fmt.Errorf("failed to write to file: %w", err)
	}

	return nil
}

func main() {
	err := writeGreeting("output.txt")
	if err != nil {
		fmt.Println("Error:", err)
	} else {
		fmt.Println("File written successfully.")
	}
}
```