package main

import (
	"os"
)

func write_greeting(filename string) error {
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer f.Close()

	if _, err := f.WriteString("Hello from the program!"); err != nil {
		return err
	}

	return nil
}
