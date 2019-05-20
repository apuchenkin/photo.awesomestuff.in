module.exports = {
  "hostname": process.env.HOSTNAME || "http://localhost:3001",
  "apiEndpoint": "/api/v1",
  "staticEndpoint": "/static",
  "analytics": process.env.ANALYTICS || false,
  "title": "PHOTO.AWESOMESTUFF.IN",
  "locales": ["ru", "en"],
  "fallbackLocale" : "en",
  "gutter": 10,
  "brickWidth": 100,
  "transition": 175,
}
