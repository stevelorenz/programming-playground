use actix_web::{web, App, HttpResponse, HttpServer};
use serde::Deserialize;

fn gcd(mut n: u64, mut m: u64) -> u64 {
    assert!(n != 0 && m != 0);
    while m != 0 {
        if m < n {
            let t = m;
            m = n;
            n = t;
        }
        m = m % n;
    }
    n
}

async fn get_index() -> HttpResponse {
    return HttpResponse::Ok().content_type("text/html").body(
        r#"
            <title>GCD Calculator</title>
            <form action="/gcd" method="post">
            <input type="text" name="n"/>
            <input type="text" name="m"/>
            <button type="submit">Compute GCD</button>
            </form>
        "#,
    );
}

#[derive(Deserialize)]
struct GcdParameters {
    n: u64,
    m: u64,
}

async fn post_gcd(form: web::Form<GcdParameters>) -> HttpResponse {
    if form.n <= 0 || form.m == 0 {
        return HttpResponse::BadRequest()
            .content_type("text/html")
            .body("Can not calculate GCD for zero!");
    }
    let response = format!(
        "The greatest common divisor of the numbers {} and {} \
                 is <b>{}</b>\n",
        form.n,
        form.m,
        gcd(form.n, form.m)
    );
    return HttpResponse::Ok().content_type("text/html").body(response);
}

#[actix_web::main]
async fn main() {
    // Here a closure/lambda function is passed to the HttpServer::new() function
    // This lambda does not take any arguments, so there's an empty ||
    let server = HttpServer::new(|| {
        // The route method of App returns the App instance with new route added!
        // So we can "pipe" the calling of route to add multiple routes into the App struct.
        return App::new()
            .route("/", web::get().to(get_index))
            .route("/gcd", web::post().to(post_gcd));
    });
    let addr = "127.0.0.1:3000";
    println!("Start serving on address: {}", addr);
    server
        .bind(addr)
        .expect("Failed to bind the server to address")
        .run()
        .await
        .expect("Failed to run the HTTP server!");
}
