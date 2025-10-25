// 示例 6: 标准库风格的包
// 模仿 Go 标准库的包命名和结构

package httputil

import (
	"net/http"
	"time"
)

// Client 表示 HTTP 客户端配置
type Client struct {
	Timeout time.Duration
	client  *http.Client
}

// NewClient 创建新的 HTTP 客户端
func NewClient(timeout time.Duration) *Client {
	return &Client{
		Timeout: timeout,
		client: &http.Client{
			Timeout: timeout,
		},
	}
}

// Get 执行 GET 请求
func (c *Client) Get(url string) (*http.Response, error) {
	return c.client.Get(url)
}
