use miette::miette;
use reqwest;
use std::env;

pub async fn send_program(prog: String) -> Result<String, miette::Report> {
  let auth_token = env::var("AUTH_TOKEN").expect("AUTH_TOKEN must be set");

  let client = reqwest::Client::builder()
    .build()
    .map_err(|e| miette!("Failed to build request client: {}", e))?;

  let result = client
    .post("https://boundvariable.space/communicate")
    .bearer_auth(&auth_token)
    .body(prog)
    .send()
    .await
    .map_err(|e| miette!("Failed to send request: {}", e))?;

  Ok(
    result
      .text()
      .await
      .map_err(|e| miette!("Failed to read response: {}", e))?,
  )
}
