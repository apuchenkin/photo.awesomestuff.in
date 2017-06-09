import React from 'react';
import { Link } from 'react-router-dom';

export default ({ categoryName }) => (
  <div className="translation">
    <div className="toolbox">
      ru, en
      <div className="tools">
        <Link to={`/category/${categoryName}`} >
          <button className="material-icons">
            clear
          </button>
        </Link>
      </div>
    </div>
    <div>translation</div>;
  </div>
);
