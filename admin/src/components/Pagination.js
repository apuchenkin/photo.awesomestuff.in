import React from 'react';
import queryString from 'query-string';
import { NavLink, withRouter } from 'react-router-dom';

const Pagination = ({
  limit = 50,
  total,
  match,
  location,
}) => (
  <div className="pagination">
    {
      Array(Math.ceil(total / limit)).fill().map((_, i) => {
        const page = i + 1;

        return (
          <NavLink
            isActive={() => queryString.parse(location.search).page === page}
            activeClassName="active"
            to={`${match.url}?page=${page}`}
          >
            {page}
          </NavLink>
        );
      })
    }
  </div>
);

export default withRouter(Pagination);
