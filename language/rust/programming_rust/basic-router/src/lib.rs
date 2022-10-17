#![allow(dead_code, unused_imports, unused_variables)]

use std::collections::HashMap;

struct Request {
    method: String,
    url: String,
    headers: HashMap<String, String>,
    body: Vec<u8>,
}

struct Response {
    code: u32,
    headers: HashMap<String, String>,
    body: Vec<u8>,
}

// Use trait objects because each closure has its own type and it's different
type BoxedCallback = Box<dyn Fn(&Request) -> Response>;

struct BasicRouter {
    routes: HashMap<String, BoxedCallback>,
}

impl BasicRouter {
    fn new() -> BasicRouter {
        return BasicRouter {
            routes: HashMap::new(),
        };
    }

    // Add route with generic type: functions or closures
    // here the static is a trait bound, it means that the type does not contain
    // any non-static references.
    fn add_route<C>(&mut self, url: &str, callback: C)
    where
        // With a Fn trait, this function accept both closures and function pointers
        C: Fn(&Request) -> Response + 'static,
    {
        self.routes.insert(url.to_string(), Box::new(callback));
    }
}

impl BasicRouter {
    fn handle_request(&self, request: &Request) -> Response {
        match self.routes.get(&request.url) {
            None => not_found_response(),
            Some(callback) => callback(request),
        }
    }
}

fn not_found_response() -> Response {
    return Response {
        code: 404,
        headers: HashMap::new(),
        body: b"<h1>Page Not Found</h1>".to_vec(),
    };
}

fn get_form_response() -> Response {
    return Response {
        code: 200,
        headers: HashMap::new(),
        body: b"<form>".to_vec(),
    };
}

fn get_gcd_response(_req: &Request) -> Response {
    return Response {
        code: 500,
        headers: HashMap::new(),
        body: b"<h1>Internal server error</h1>".to_vec(),
    };
}

fn req(url: &str) -> Request {
    return Request {
        method: "GET".to_string(),
        url: url.to_string(),
        headers: HashMap::new(),
        body: vec![],
    };
}

#[test]
fn test_router() {
    let mut router = BasicRouter::new();

    router.add_route("/", |_| get_form_response());
    router.add_route("/gcd", |req| get_gcd_response(req));
    router.add_route("/apink", |_| {
        return Response {
            code: 666,
            headers: HashMap::new(),
            body: b"Apink!".to_vec(),
        };
    });

    assert_eq!(router.handle_request(&req("/")).code, 200);
    assert_eq!(router.handle_request(&req("/piano")).code, 404);
    assert_eq!(router.handle_request(&req("/gcd")).code, 500);
    assert_eq!(router.handle_request(&req("/apink")).code, 666);
}
