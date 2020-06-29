package main

import (
	"fmt"
	"time"
)

// The worker function only accpets a channel for sending values.
func worker(done chan<- bool) {
	fmt.Print("working...")
	time.Sleep(time.Second)
	fmt.Println("done")
	done <- true
}

func main() {
	messages := make(chan string)
	go func() { messages <- "ping" }()
	// By default, sends and receives block until both the sender and receiver are ready.
	msg := <-messages
	fmt.Println(msg)

	buffered_messages := make(chan string, 2)
	// We can send up to 2 values into the channel without a corresponding concurrent receive
	buffered_messages <- "buffered"
	buffered_messages <- "channel"

	fmt.Println(<-buffered_messages)
	fmt.Println(<-buffered_messages)

	// Use a blocking receive to wait for a goroutine to finish
	done := make(chan bool, 1)
	go worker(done)
	<-done
}
