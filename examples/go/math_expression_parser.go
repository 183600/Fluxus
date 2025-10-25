package main

import (
	"fmt"
	"math"
	"strconv"
	"strings"
)

type MathExpression struct {
	expression string
	tokens     []string
	position   int
}

func NewMathExpression(expr string) *MathExpression {
	// Remove spaces and tokenize
	expr = strings.ReplaceAll(expr, " ", "")
	tokens := tokenize(expr)
	return &MathExpression{
		expression: expr,
		tokens:     tokens,
		position:   0,
	}
}

func tokenize(expr string) []string {
	var tokens []string
	var current strings.Builder

	for i, char := range expr {
		if char >= '0' && char <= '9' || char == '.' {
			current.WriteRune(char)
		} else {
			if current.Len() > 0 {
				tokens = append(tokens, current.String())
				current.Reset()
			}
			tokens = append(tokens, string(char))
		}
		
		// Handle last number
		if i == len(expr)-1 && current.Len() > 0 {
			tokens = append(tokens, current.String())
		}
	}
	
	return tokens
}

func (me *MathExpression) parse() (float64, error) {
	me.position = 0
	return me.parseExpression()
}

func (me *MathExpression) parseExpression() (float64, error) {
	left, err := me.parseTerm()
	if err != nil {
		return 0, err
	}

	for me.position < len(me.tokens) {
		operator := me.tokens[me.position]
		if operator != "+" && operator != "-" {
			break
		}
		me.position++

		right, err := me.parseTerm()
		if err != nil {
			return 0, err
		}

		if operator == "+" {
			left += right
		} else {
			left -= right
		}
	}

	return left, nil
}

func (me *MathExpression) parseTerm() (float64, error) {
	left, err := me.parseFactor()
	if err != nil {
		return 0, err
	}

	for me.position < len(me.tokens) {
		operator := me.tokens[me.position]
		if operator != "*" && operator != "/" && operator != "%" {
			break
		}
		me.position++

		right, err := me.parseFactor()
		if err != nil {
			return 0, err
		}

		switch operator {
		case "*":
			left *= right
		case "/":
			if right == 0 {
				return 0, fmt.Errorf("division by zero")
			}
			left /= right
		case "%":
			if right == 0 {
				return 0, fmt.Errorf("modulo by zero")
			}
			left = math.Mod(left, right)
		}
	}

	return left, nil
}

func (me *MathExpression) parseFactor() (float64, error) {
	if me.position >= len(me.tokens) {
		return 0, fmt.Errorf("unexpected end of expression")
	}

	token := me.tokens[me.position]
	me.position++

	// Handle parentheses
	if token == "(" {
		result, err := me.parseExpression()
		if err != nil {
			return 0, err
		}
		if me.position >= len(me.tokens) || me.tokens[me.position] != ")" {
			return 0, fmt.Errorf("missing closing parenthesis")
		}
		me.position++
		return result, nil
	}

	// Handle unary minus
	if token == "-" {
		value, err := me.parseFactor()
		if err != nil {
			return 0, err
		}
		return -value, nil
	}

	// Handle numbers
	value, err := strconv.ParseFloat(token, 64)
	if err != nil {
		return 0, fmt.Errorf("invalid number: %s", token)
	}

	return value, nil
}

type Calculator struct {
	history []string
}

func NewCalculator() *Calculator {
	return &Calculator{
		history: make([]string, 0),
	}
}

func (c *Calculator) Calculate(expression string) (float64, error) {
	mathExpr := NewMathExpression(expression)
	result, err := mathExpr.parse()
	
	if err == nil {
		record := fmt.Sprintf("%s = %.6f", expression, result)
		c.history = append(c.history, record)
	}
	
	return result, err
}

func (c *Calculator) GetHistory() []string {
	return c.history
}

func (c *Calculator) ClearHistory() {
	c.history = make([]string, 0)
}

func main() {
	fmt.Println("Advanced Mathematical Expression Parser")
	fmt.Println("======================================")

	calc := NewCalculator()

	expressions := []string{
		"2 + 3 * 4",
		"(2 + 3) * 4",
		"10 / 2 + 3",
		"2 * (3 + 4) * 5",
		"100 - 50 / 2",
		"((2 + 3) * (4 + 5)) / 3",
		"15 % 4 + 2",
		"-5 + 10",
		"2.5 * 4.2 + 1.8",
		"((10 + 5) * 2) - (8 / 4)",
	}

	for _, expr := range expressions {
		result, err := calc.Calculate(expr)
		if err != nil {
			fmt.Printf("Error calculating '%s': %v\n", expr, err)
		} else {
			fmt.Printf("%s = %.6f\n", expr, result)
		}
	}

	fmt.Println("\nCalculation History:")
	fmt.Println("===================")
	for i, record := range calc.GetHistory() {
		fmt.Printf("%d. %s\n", i+1, record)
	}

	fmt.Printf("\nTotal calculations: %d\n", len(calc.GetHistory()))

	// Test error cases
	fmt.Println("\nTesting Error Cases:")
	fmt.Println("===================")
	errorCases := []string{
		"10 / 0",
		"5 + * 3",
		"(2 + 3",
		"abc + 5",
		"",
	}

	for _, expr := range errorCases {
		_, err := calc.Calculate(expr)
		if err != nil {
			fmt.Printf("'%s' -> Error: %v\n", expr, err)
		}
	}
}