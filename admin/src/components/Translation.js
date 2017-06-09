import React from 'react';
import { Link } from 'react-router-dom';

class Translation extends React.Component {
  constructor(props) {
    super(props);

    this.state = {
      translations: [],
    };
  }

  componentWillMount() {
    const { categoryName, admin } = this.props;

    admin.categoryService.fetchTranslations(categoryName)
      .then((translations) => {
        this.setState({ translations });
      });
  }

  render() {
    const { categoryName } = this.props;
    const { translations } = this.state;

    return (
      <div className="translation">
        <div className="toolbox">
          Translations
          <div className="tools">
            <Link to={`/category/${categoryName}`} >
              <button className="material-icons">
                clear
              </button>
            </Link>
          </div>
        </div>
        <div>{ translations }</div>;
      </div>
    );
  }
}

export default Translation;
