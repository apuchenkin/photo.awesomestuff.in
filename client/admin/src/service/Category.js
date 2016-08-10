
const CategoryService = class {

  constructor(token) {
    this.token = token;
  }

  fetchCategories () {
    let me = this;

    return fetch('/api/v1/category')
        .then(response => {
          return response.text();
        })
        .then(stream => {
          return JSON.parse(stream);
        })
  }
};

export default CategoryService;
