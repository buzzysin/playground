package main

import (
	"encoding/json"
	"log"
	"net/http"
	"sort"
)

// simple API: GET / -> hello
// GET /sort?nums=3,1,2 -> returns sorted JSON

func main() {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "text/plain; charset=utf-8")
		w.Write([]byte("Hello, Go playground!\n"))
	})

	http.HandleFunc("/sort", func(w http.ResponseWriter, r *http.Request) {
		q := r.URL.Query().Get("nums")
		var nums []int
		if q == "" {
			// default
			nums = []int{5, 3, 8, 1}
		} else {
			if err := json.Unmarshal([]byte(q), &nums); err != nil {
				// also support comma-separated as fallback
				var tmp []int
				if err2 := parseCSVInts(q, &tmp); err2 != nil {
					http.Error(w, "bad nums param", http.StatusBadRequest)
					return
				}
				nums = tmp
			}
		}
		sort.Ints(nums)
		w.Header().Set("Content-Type", "application/json; charset=utf-8")
		_ = json.NewEncoder(w).Encode(nums)
	})

	log.Println("Starting Go playground HTTP server on :8080")
	log.Fatal(http.ListenAndServe(":8080", nil))
}

func parseCSVInts(s string, out *[]int) error {
	var cur int
	var neg bool
	*out = (*out)[:0]
	num := 0
	have := false
	for i := 0; i < len(s); i++ {
		c := s[i]
		if c >= '0' && c <= '9' {
			have = true
			num = num*10 + int(c-'0')
			continue
		}
		if c == '-' {
			neg = true
			continue
		}
		if c == ',' || c == ' ' {
			if have {
				if neg { cur = -num } else { cur = num }
				*out = append(*out, cur)
				num = 0
				have = false
				neg = false
			}
			continue
		}
		// unexpected char
		return nil
	}
	if have {
		if neg { cur = -num } else { cur = num }
		*out = append(*out, cur)
	}
	return nil
}
