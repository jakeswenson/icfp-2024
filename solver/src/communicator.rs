use reqwest;
use dotenvy::dotenv;
use std::env;

pub async fn send_program(prog: String) -> Result<(), reqwest::Error> {

  dotenv().ok();
  let auth_token = env::var("AUTH_TOKEN").expect("AUTH_TOKEN must be set");

  let client = reqwest::Client::builder()
    .build()?;

  let result = client
    .post("https://boundvariable.space/communicate")
    .bearer_auth(&auth_token)
    .body(prog)
    .send()
    .await?;

  println!("{:?}", result.text().await?);

  Ok(())
}