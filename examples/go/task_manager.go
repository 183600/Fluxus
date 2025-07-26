package main

import (
	"fmt"
	"time"
)

type Task struct {
	ID          int
	Title       string
	Description string
	Priority    string
	Status      string
	CreatedAt   time.Time
	CompletedAt *time.Time
}

type TaskManager struct {
	tasks  []Task
	nextID int
}

func NewTaskManager() *TaskManager {
	return &TaskManager{
		tasks:  make([]Task, 0),
		nextID: 1,
	}
}

func (tm *TaskManager) AddTask(title, description, priority string) {
	task := Task{
		ID:          tm.nextID,
		Title:       title,
		Description: description,
		Priority:    priority,
		Status:      "pending",
		CreatedAt:   time.Now(),
		CompletedAt: nil,
	}
	tm.tasks = append(tm.tasks, task)
	tm.nextID++
	fmt.Printf("Task added: %s (ID: %d)\n", title, task.ID)
}

func (tm *TaskManager) CompleteTask(id int) {
	for i := range tm.tasks {
		if tm.tasks[i].ID == id {
			tm.tasks[i].Status = "completed"
			now := time.Now()
			tm.tasks[i].CompletedAt = &now
			fmt.Printf("Task completed: %s\n", tm.tasks[i].Title)
			return
		}
	}
	fmt.Printf("Task with ID %d not found\n", id)
}

func (tm *TaskManager) ListTasks() {
	fmt.Println("\n=== Task List ===")
	for _, task := range tm.tasks {
		status := task.Status
		if task.CompletedAt != nil {
			status += fmt.Sprintf(" (completed at %s)", 
				task.CompletedAt.Format("2006-01-02 15:04:05"))
		}
		fmt.Printf("ID: %d | %s | Priority: %s | Status: %s\n", 
			task.ID, task.Title, task.Priority, status)
		fmt.Printf("  Description: %s\n", task.Description)
		fmt.Printf("  Created: %s\n", task.CreatedAt.Format("2006-01-02 15:04:05"))
		fmt.Println()
	}
}

func (tm *TaskManager) GetTasksByStatus(status string) []Task {
	var filtered []Task
	for _, task := range tm.tasks {
		if task.Status == status {
			filtered = append(filtered, task)
		}
	}
	return filtered
}

func (tm *TaskManager) GetTasksByPriority(priority string) []Task {
	var filtered []Task
	for _, task := range tm.tasks {
		if task.Priority == priority {
			filtered = append(filtered, task)
		}
	}
	return filtered
}

func main() {
	fmt.Println("Task Management System")
	
	tm := NewTaskManager()
	
	tm.AddTask("Complete project proposal", "Write and submit the Q4 project proposal", "high")
	tm.AddTask("Review code", "Review pull requests from team members", "medium")
	tm.AddTask("Update documentation", "Update API documentation with new endpoints", "low")
	tm.AddTask("Fix critical bug", "Fix the authentication issue in production", "high")
	tm.AddTask("Team meeting prep", "Prepare agenda for weekly team meeting", "medium")
	
	tm.ListTasks()
	
	tm.CompleteTask(1)
	tm.CompleteTask(4)
	
	fmt.Println("\nHigh priority tasks:")
	highPriorityTasks := tm.GetTasksByPriority("high")
	for _, task := range highPriorityTasks {
		fmt.Printf("- %s (%s)\n", task.Title, task.Status)
	}
	
	fmt.Println("\nPending tasks:")
	pendingTasks := tm.GetTasksByStatus("pending")
	for _, task := range pendingTasks {
		fmt.Printf("- %s (Priority: %s)\n", task.Title, task.Priority)
	}
}