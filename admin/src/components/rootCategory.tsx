import * as React from 'react';
import Category from './Category';

interface Props {
  category: Category;
}

const RootCategory: React.FunctionComponent<Props> = ({ category }) => {
  const [expanded, setExpanded] = React.useState(false);

  return (
    <li className="item">
      <button
        className="material-icons"
        onClick={() => setExpanded(!expanded)}
        disabled={!category.children.length}
      >{
        expanded
          ? 'keyboard_arrow_up'
          : 'keyboard_arrow_down'
      }</button>
      <Category category={category} />
      {
        expanded && (
          <ul className="childs">
            {category.children.map(child => (
              <li className="item" key={child.id} >
                <Category category={child} />
              </li>
            ))}
          </ul>
        )
      }
    </li>
  );
}

export default RootCategory;
