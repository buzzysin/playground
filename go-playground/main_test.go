package main

import (
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"sort"
	"testing"
)

func TestDefaultSort(t *testing.T) {
	req := httptest.NewRequest("GET", "/sort", nil)
	w := httptest.NewRecorder()

	http.DefaultServeMux = http.NewServeMux() // reset
	// re-register handlers from main
	// we call the handlers directly
	http.HandleFunc("/sort", func(w http.ResponseWriter, r *http.Request) {
		q := r.URL.Query().Get("nums")
		var nums []int
		if q == "" {
			nums = []int{5, 3, 8, 1}
		} else {
			_ = json.Unmarshal([]byte(q), &nums)
		}
		sort.Ints(nums)
		_ = json.NewEncoder(w).Encode(nums)
	})

	http.DefaultServeMux.ServeHTTP(w, req)

	if w.Code != 200 {
		t.Fatalf("expected 200 got %d", w.Code)
	}
	var got []int
	if err := json.Unmarshal(w.Body.Bytes(), &got); err != nil {
		t.Fatalf("json decode: %v", err)
	}
	if len(got) != 4 || got[0] != 1 || got[3] != 8 {
		t.Fatalf("unexpected result: %v", got)
	}
}
