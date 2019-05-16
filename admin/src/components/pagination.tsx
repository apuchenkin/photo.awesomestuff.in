import * as React from 'react';
import * as queryString from 'query-string';
import { NavLink } from 'react-router-dom';
import { __RouterContext } from 'react-router';
import { range } from 'ramda';

interface Props {
  total: number;
  limit?: number;
}

const Pagination: React.FunctionComponent<Props> = ({
  limit = 50,
  total,
}) => {
  const { match, location } = React.useContext(__RouterContext);

  return (
    <div className="pagination">
      {
        range(1, Math.ceil(total / limit) + 1).map((page) => (
          <NavLink
            key={page}
            isActive={() => Number(queryString.parse(location.search).page) === page}
            activeClassName="active"
            to={`${match.url}?page=${page}`}
          >
            {page}
          </NavLink>
        ))
      }
    </div>
  )
};

export default Pagination;
